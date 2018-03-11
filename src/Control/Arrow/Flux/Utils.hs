module Control.Arrow.Flux.Utils where

import           Control.Arrow.Flux

inspect :: (Show a)
        => Flux a a
inspect = Flux $ \x -> print x >> pure (x, inspect)
