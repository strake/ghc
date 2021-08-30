{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Utils.Lens.Monad.Zoom where

import GHC.Prelude

import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Writer (WriterT (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.State.Class
import Data.Kind (Type)
import Lens.Micro (LensLike')

class (IsState f, IsState g) => Zoom f g where
    zoom :: LensLike' (Zoomed f a) (StateType g) (StateType f) -> f a -> g a

instance (Zoom f g) => Zoom (ExceptT e f) (ExceptT e g) where
    zoom l (ExceptT x) = ExceptT (zoom (\ f -> unfocusingOn . l (FocusingOn . f)) x)

instance (Zoom f g) => Zoom (IdentityT f) (IdentityT g) where
    zoom l (IdentityT x) = IdentityT (zoom l x)

instance (Applicative f, Zoom f g) => Zoom (MaybeT f) (MaybeT g) where
    zoom l (MaybeT x) = MaybeT (zoom (\ f -> unfocusingOn . l (FocusingOn . f)) x)

instance (Zoom f g) => Zoom (ReaderT r f) (ReaderT r g) where
    zoom l (ReaderT x) = ReaderT (zoom l . x)

instance Applicative f => Zoom (StateT s f) (StateT t f) where
    zoom l (StateT x) = StateT (unfocusing . l (Focusing . x))

instance (Zoom f g, Monoid w) => Zoom (WriterT w f) (WriterT w g) where
    zoom l (WriterT x) = WriterT (zoom (\ f -> unfocusingPlus . l (FocusingPlus . f)) x)

type family Zoomed (f :: Type -> Type) :: Type -> Type -> Type
type instance Zoomed (ExceptT e f) = FocusingOn (Either e) (Zoomed f)
type instance Zoomed (IdentityT f) = Zoomed f
type instance Zoomed (MaybeT f) = FocusingOn Maybe (Zoomed f)
type instance Zoomed (ReaderT r f) = Zoomed f
type instance Zoomed (StateT s f) = Focusing f
type instance Zoomed (WriterT w f) = FocusingPlus w (Zoomed f)

newtype Focusing f s a = Focusing { unfocusing :: f (s, a) }
  deriving stock (Functor)
  deriving (Applicative) via Compose f ((,) s)

newtype FocusingOn g f s a = FocusingOn { unfocusingOn :: f (g s) a }
  deriving stock (Functor)
  deriving (Applicative) via f (g s)

newtype FocusingPlus w f s a = FocusingPlus { unfocusingPlus :: f (s, w) a }

deriving via (f :: Type -> Type -> Type) (s, w) instance Functor (f (s, w)) => Functor (FocusingPlus w f s)
deriving via (f :: Type -> Type -> Type) (s, w) instance Applicative (f (s, w)) => Applicative (FocusingPlus w f s)
