{-# LANGUAGE RankNTypes #-}

module Traversal (traverseOf, both, traverseList) where

import Data.Functor.Const
import Data.Functor.Identity

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

traverseOf :: Applicative f => Traversal s t a b -> (a -> f b) -> s -> f t
traverseOf = id

both :: Traversal (a,a) (b,b) a b
both f (x,y) = (,) <$> f x <*> f y

traverseList :: Traversal [a] [b] a b
traverseList _ [] = pure []
traverseList f (x:xs) = (:) <$> f x <*> traverseList f xs


(%~) :: Traversal s t a b -> (a -> Identity b) -> s -> t
traversal %~ f  = runIdentity . traversal f