module Data.Flux
  ( Flux(..)
  , Producer(..)
  , Consumer(..)
  , Source
  , source
  , input
  , Sink
  , sink
  , output
  , select
  , flux
  , delay
  , forever
  ) where

import           Control.Arrow
import           Control.Category
import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TChan as T
import           Control.Monad                (void)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Void                    (Void)
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
source s = do
  (r, w) <- newChan
  forkIO (step w s)
  pure r
  where
    step w (Producer s) = do
      (x, next) <- s
      w <! x
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

flux :: Source i -- ^ Can be reused
     -> Flux i o
     -> IO (Source o)
flux (Source r) f = do
  rd <- atomically $ T.dupTChan r
  (r', w) <- newChan
  plug (Source rd) w f
  pure r'

plug :: Source i -> Sink o -> Flux i o -> IO ()
plug r w f = void $ forkIO (void $ flux r w f)
  where flux :: Source i -> Sink o -> Flux i o -> IO Forever
        flux r w (Flux f) = do
          x <- readSource r
          (y, f') <- f x
          w <! y
          flux r w f'

forever :: Flux () ()
       -> IO ()
forever (Flux f) = do
  next <- snd <$> f ()
  forever next

delay :: a
      -> Flux a a
delay x = Flux $ \y ->
  pure (x, delay y)

{-
instance ArrowLoop Flux where
  loop (Flux f) = Flux $ \x -> do
    ((y, d), next) <- f (x, d)
    pure (y, loop next)
-}
