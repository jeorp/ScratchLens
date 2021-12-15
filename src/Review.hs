{-# LANGUAGE RankNTypes #-}

module Review where

import Data.Functor.Const
import Data.Functor.Identity
import Control.Arrow
import Data.Profunctor
import Data.Tagged

import Lens

type Review s a = forall p f. (Applicative f) => p a (f a) -> p s (f s)
type AReview s a = Tagged a (Identity a) -> Tagged s (Identity s)

review :: AReview s a -> a -> s
review r = runIdentity  . unTagged  . r . Tagged . Identity

re :: AReview s a -> Getter' r a s
re = to . review 

