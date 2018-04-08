{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Control.Arrow.Flux.Utils where

import           Control.Arrow.Flux
import           Control.Monad      ((>=>))

inspect :: (Show a)
        => Flux a a
inspect = Flux $ \x -> print x >> pure (x, inspect)

class Fluxize x where
  type Input x
  type Output x

  io :: x -> Flux (Input x) (Output x)

instance Fluxize (IO a) where
  type Input (IO a) = ()
  type Output (IO a) = a

  io c = Flux . pure $ c >>= \x -> pure (x, io c)

instance Fluxize (a -> IO b) where
  type Input (a -> IO b) = a
  type Output (a -> IO b) = b

  io f = Flux $ f >=> \y -> pure (y, io f)

instance Fluxize (a -> b -> IO c) where
  type Input (a -> b -> IO c) = (a, b)
  type Output (a -> b -> IO c) = c

  io f = Flux $ \(x, y) -> f x y >>= \u -> pure (u, io f)

instance Fluxize (a -> b -> c -> IO d) where
  type Input (a -> b -> c -> IO d) = (a, b, c)
  type Output (a -> b -> c -> IO d) = d

  io f = Flux $ \(x, y, z) -> f x y z >>= \u -> pure (u, io f)

instance Fluxize (a -> b -> c -> d -> IO e) where
  type Input (a -> b -> c -> d -> IO e) = (a, b, c, d)
  type Output (a -> b -> c -> d -> IO e) = e

  io f = Flux $ \(w, x, y, z) -> f w x y z >>= \u -> pure (u, io f)
