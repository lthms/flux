module Data.Flux
  ( Producer(..)
  , Consumer(..)
  , Flux(..)
  , Source
  , Sink
  -- * Podes
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
  -- * Flux
  , constant
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
import           Control.Monad                (join, mapM_, void)
import           Control.Monad.Fix
import           Prelude                      hiding (id, (.))

data Forever

newtype Source a = Source { unSource :: T.TChan a }

readSource :: Source a -> IO a
readSource (Source chan) = atomically $ T.readTChan chan

select :: [Source a] -- ^ Cannot be reused
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

constant :: o -> Flux i o
constant x = Flux $ \_ -> pure (x, constant x)

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

newtype Producer i = Producer (IO ([i], Producer i))

instance Functor Producer where
  fmap f (Producer p) = Producer $ do
    (x, next) <- p
    pure (f <$> x, fmap f next)

instance Applicative Producer where
  (Producer pf) <*> (Producer px) = Producer $ do
    af <- async pf
    ax <- async px
    (f, fnext) <- wait af
    (x, xnext) <- wait ax
    pure (f <*> x, fnext <*> xnext)

  pure x = Producer $ pure ([x], pure x)

source :: Producer i
       -> IO (Source i)
source s = do
  (r, w) <- newChan
  forkIO (step w s)
  pure r
  where
    step w (Producer s) = do
      (lx, next) <- s
      mapM_ (w <!) lx
      step w next

input :: Source i
      -> Flux () i
input s = Flux $ \_ -> do
  x <- readSource s
  pure (x, input s)

newtype Consumer o = Consumer (o -> IO (Consumer o))

sink :: Consumer o
     -> IO (Sink o)
sink c = do
  (r, w) <- newChan
  forkIO (step r c)
  pure w
  where
    step r (Consumer c) = do
      x <- readSource r
      next <- c x
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

delay :: a
      -> Flux a a
delay x = Flux $ \y ->
  pure (x, delay y)

app2 :: (o -> o -> o)
     -> Flux i o
     -> Flux i o
     -> Flux i o
app2 h (Flux f) (Flux g) = Flux $ \x -> do
    ((a, nextf), (b, nextg)) <- join $ waitBoth <$> async (f x) <*> async (g x)
    pure (h a b, app2 h nextf nextg)

instance (Num o) => Num (Flux i o) where
  (+) = app2 (+)
  (*) = app2 (*)
  negate = (>>> arr negate)
  signum = (>>> arr signum)
  fromInteger = fromInteger >>> constant
  abs = (>>> arr abs)

instance (Monoid o) => Monoid (Flux i o) where
  mempty = constant mempty
  mappend = app2 mappend

{-
instance ArrowLoop Flux where
  loop (Flux f) = Flux $ \x -> do
    ((y, d), next) <- f (x, d)
    pure (y, loop next)
-}
