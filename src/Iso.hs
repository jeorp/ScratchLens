{-# LANGUAGE RankNTypes #-}

module Iso (Equality, Iso) where

import Control.Arrow
import Data.Profunctor

type Equality s t a b = forall p f. p a (f b) -> p s (f t)

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = dimap f (fmap g) 