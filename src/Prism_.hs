{-# LANGUAGE RankNTypes #-}
module Prism_ (Prism_, prism_, _Left_, _Right_) where

type Prism_ s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

prism_ :: (b -> t) -> (s -> Either t a) -> Prism_ s t a b
prism_ bt g f s = case g s of
    Left t -> pure t
    Right a -> bt <$> f a

_Left_ :: Prism_ (Either a c) (Either b c) a b
_Left_ f s = case s of
    Left a -> Left <$> f a
    Right c -> pure $ Right c

_Right_ :: Prism_ (Either c a) (Either c b) a b
_Right_ f s = case s of
    Left c -> pure $ Left c
    Right a -> Right <$> f a
