{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module Control.Arrow.Flux
  ( Producer(..)
  , repeatP
  , fluxP
  , enumP
  , Consumer(..)
  , repeatC
  , fluxC
  , enumC
  , Flux(..)
  , (->>)
  , (*->>)
  , (*->>*)
  , (>>-)
  , (*>>-)
  -- * Podes
  , Source
  , Sink
  -- ** Concurrent
  -- | This will spawn a new thread.
  , source
  , select
  , sink
  , plugR
  , plugL
  , plug
  , pipe
  -- ** Local
  -- | This will use the current thread (and therefore blocks).
  , forever
  , asLongAs
  , once
  -- * Flux
  , input
  , output
  , delay
  ) where

import           Control.Arrow
import           Control.Category
import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TChan as T
import           Control.Monad                (join, mapM_, void, when)
import           Data.Foldable                (toList)
import           Prelude                      hiding (id, (.))

newtype Source a = Source { unSource :: T.TChan a }

readSource :: Source a -> IO a
readSource (Source chan) = atomically $ T.readTChan chan

select :: [Source a]
       -> Flux () a
select srcs = Flux $ \_ -> do
  x <- atomically $ foldl1 orElse (T.readTChan . unSource <$> srcs)
  pure (x, select srcs)

newtype Sink a = Sink (T.TChan a)

(<!) :: Sink a -> a -> IO ()
(Sink chan) <! x = atomically $ T.writeTChan chan x

newChan :: IO (Source a, Sink a)
newChan = do
  w <- T.newBroadcastTChanIO
  r <- atomically $ T.dupTChan w

  pure (Source r, Sink w)

newtype Flux i o = Flux (i -> IO (o, Flux i o))

instance Functor (Flux i) where
  fmap f (Flux g) = Flux $ \x -> do
    (y, next) <- g x
    pure (f y, fmap f next)

instance Applicative (Flux i) where
  pure x = Flux $ \_ -> pure (x, pure x)

  (Flux f) <*> (Flux g) = Flux $ \x -> do
    ((h, nextf), (y, nextg)) <- join $ waitBoth <$> async (f x) <*> async (g x)
    pure (h y, nextf <*> nextg)

instance Category Flux where
  id = Flux $ \i -> pure (i, id)

  (Flux g) . (Flux f) = Flux $ \i -> do
    (x, f') <- f i
    (y, g') <- g x
    pure (y, g' . f')

instance Arrow Flux where
  arr f = Flux $ \i -> pure (f i, arr f)

  (Flux f) *** (Flux g) = Flux $ \(a, b) -> do
    pf <- async $ f a
    pg <- async $ g b
    (x, f') <- wait pf
    (y, g') <- wait pg
    pure ((x, y), f' *** g')

newtype Producer i = Producer (IO (i, Producer i))

repeatP :: IO i -> Producer i
repeatP p = Producer $ p >>= \i -> pure (i, repeatP p)

fluxP :: Flux () i -> Producer i
fluxP (Flux f) = Producer $ do
  (x, fnext) <- f ()
  pure (x, fluxP fnext)

(->>) :: Producer i -> Flux i j -> Producer j
(Producer p) ->> (Flux f) = Producer $ do
  (x, nextp) <- p
  (y, nextf) <- f x
  pure (y, nextp ->> nextf)

(*->>*) :: (Foldable f) => Producer (f i) -> Flux i j -> Producer [j]
(Producer p) *->>* f = Producer $ do
  (x, nextp) <- p
  (y, nextf) <- process (toList x) [] f
  pure (y, nextp *->>* nextf)
  where
    process :: [i] -> [j] -> Flux i j -> IO ([j], Flux i j)
    process (x:rst) res (Flux f) = do
      (y, nextf) <- f x
      process rst (y:res) nextf
    process [] res f = pure (reverse res, f)

(*->>) :: (Foldable f) => Producer (f i) -> Flux i j -> Producer j
p *->> f = enumP (p *->>* f)

enumP :: (Foldable f) => Producer (f i) -> Producer i
enumP p = Producer $ aux [] (toList <$> p)
  where
    aux :: [i] -> Producer [i] -> IO (i, Producer i)
    aux (x:rst) p       = pure (x, Producer $ aux rst p)
    aux [] (Producer p) = p >>= uncurry aux

enumC :: (Foldable f) => Consumer o -> Consumer (f o)
enumC c = Consumer $ \f -> enumC <$> aux (toList f) c
  where
    aux :: [o] -> Consumer o -> IO (Consumer o)
    aux [] c                 = pure c
    aux (x:rst) (Consumer c) = c x >>= aux rst

instance Functor Producer where
  fmap f (Producer p) = Producer $ do
    (x, next) <- p
    pure (f x, fmap f next)

instance Applicative Producer where
  (Producer pf) <*> (Producer px) = Producer $ do
    af <- async pf
    ax <- async px
    (f, fnext) <- wait af
    (x, xnext) <- wait ax
    pure (f x, fnext <*> xnext)

  pure x = Producer $ pure (x, pure x)

source :: Producer i
       -> IO (Source i)
source p = do
  (r, w) <- newChan
  void $ forkIO (step w p)
  pure r
  where
    step w (Producer prod) = do
      (x, next) <- prod
      w <! x
      step w next

input :: Source i
      -> Flux () i
input s = Flux $ \_ -> do
  x <- readSource s
  pure (x, input s)

newtype Consumer o = Consumer (o -> IO (Consumer o))

repeatC :: (o -> IO a) -> Consumer o
repeatC f = Consumer $ \x -> f x >> pure (repeatC f)

fluxC :: Flux o a -> Consumer o
fluxC (Flux f) = Consumer $ fmap (fluxC . snd) . f

(>>-) :: Flux i j -> Consumer j -> Consumer i
(Flux f) >>- (Consumer c) = Consumer $ \x -> do
  (y, nextf) <- f x
  nextc <- c y
  pure (nextf >>- nextc)

(*>>-) :: (Foldable f) => Flux i (f j) -> Consumer j -> Consumer i
(Flux f) *>>- c = Consumer $ \x -> do
  (y, nextf) <- f x
  nextc <- process (toList y) c
  pure (nextf *>>- nextc)
  where process :: [j] -> Consumer j -> IO (Consumer j)
        process (y:rst) (Consumer c) = c y >>= process rst
        process [] c                 = pure c

sink :: Consumer o
     -> IO (Sink o)
sink c = do
  (r, w) <- newChan
  void $ forkIO (step r c)
  pure w
  where
    step r (Consumer cs) = do
      x <- readSource r
      next <- cs x
      step r next

output :: Sink o
       -> Flux o ()
output w = Flux $ \x -> do
  w <! x
  pure ((), output w)

pipe :: Flux i o
     -> IO (Sink i, Source o)
pipe f = do
  (ri, wi) <- newChan
  (ro, wo) <- newChan
  plug ri f wo
  pure (wi, ro)

plugL :: Source i
      -> Flux i o
      -> IO (Source o)
plugL (Source r) f = do
  ri <- atomically $ T.dupTChan r
  (ro, wo) <- newChan
  plug (Source ri) f wo
  pure ro

plugR :: Flux i o
      -> Sink o
      -> IO (Sink i)
plugR f wo = do
  (ri, wi) <- newChan
  plug ri f wo
  pure wi

plug :: Source i
     -> Flux i o
     -> Sink o
     -> IO ()
plug r f w = void . forkIO $ step r w f
  where
    step ri wo (Flux g) = do
      x <- readSource ri
      (y, next) <- g x
      wo <! y
      step ri wo next

forever :: Flux () a
       -> IO ()
forever (Flux f) = snd <$> f () >>= forever

-- | Block the current thread as long as the Flux continue to produce 'True'.
asLongAs :: Flux () Bool -> IO ()
asLongAs (Flux f) = do
  (x, nextf) <- f ()
  when x $ asLongAs nextf

once :: i
     -> Flux i a
     -> IO a
once x (Flux f) = fst <$> f x

delay :: a
      -> Flux a a
delay x = Flux $ \y ->
  pure (x, delay y)

app2 :: (a -> b -> c)
     -> Flux i a
     -> Flux i b
     -> Flux i c
app2 h f g = h <$> f <*> g

instance (Num o) => Num (Flux i o) where
  (+) = app2 (+)
  (*) = app2 (*)
  negate = (>>> arr negate)
  signum = (>>> arr signum)
  fromInteger = fromInteger >>> pure
  abs = (>>> arr abs)

instance (Monoid o) => Monoid (Flux i o) where
  mempty = pure mempty
  mappend = app2 mappend

instance ArrowLoop Flux where
  loop (Flux f) = Flux $ \x -> do
    rec ((y, d), next) <- f (x, d)
    pure (y, loop next)

instance ArrowChoice Flux where
  (Flux f) +++ (Flux g) = Flux $ \case
    Left x -> do
      (y, nextf) <- f x
      pure (Left y, nextf +++ Flux g)
    Right x -> do
      (y, nextg) <- g x
      pure (Right y, Flux f +++ nextg)
