{-# LANGUAGE RankNTypes #-}
module Lens (Lens, Lens', Getter, Getter', Setter, Setter', to) where

import Data.Functor.Const
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' a s = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b 
lens sa sbt afb s = fmap (sbt s) (afb $ sa s)

type Getter r s t a b = (a -> Const r b) -> s -> Const r t
type Getter' r s a = Getter r s s a a

type Setter s t a b = (a -> Identity b) -> s -> Identity t
type Setter' s a = Setter s s a a 

view :: Getter a s t a b -> s -> a
view getter s = getConst $ getter Const s

(^.) :: Getter a s t a b -> s -> a
getter ^. s = view getter s

to :: (s -> a) -> Getter' r s a
to f = lens f const 

over :: (a -> b) -> Setter s t a b -> s -> t
over f setter s = runIdentity $ setter (Identity . f) s

over' :: b -> Setter s t a b -> s -> t
over' b = over $ const b

(.~) :: Setter s t a b -> b -> s -> t
setter .~ b = over' b setter



use :: MonadState s m => Getter' a s a -> m a
use getter = do
    s <- get
    pure $ getter ^. s

assign :: MonadState s m => (a -> a) -> Setter' s a -> m ()
assign f setter = do
    s <- get 
    put $ over f setter s

(%=) :: MonadState s m => Setter' s a -> (a -> a) -> m ()
(%=) = flip assign

assign' :: MonadState s m => a -> Setter' s a -> m ()
assign' a = assign (const a)

(.=) :: MonadState s m => Setter' s a -> a -> m ()
(.=) = flip assign'

viewReader :: MonadReader s m => Getter' a s a -> m a
viewReader getter = do
    s <- ask
    pure $ getter ^. s

localReader :: MonadReader s m => (a -> a) -> Setter' s a -> m b -> m b
localReader f setter = local $ over f setter



