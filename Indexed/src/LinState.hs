{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module LinState() where

import Control.Monad.Indexed
import Data.Coerce
import Fcf
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import Control.Monad.Indexed
import Language.Haskell.DoNotation
import Prelude hiding (Monad (..), pure)
import qualified System.IO as SIO
import System.IO hiding (openFile, Handle)

newtype Ix m i j a = Ix {unsafeRunIx :: m a} deriving (Functor, Applicative, Monad)

instance Functor m => IxFunctor (Ix m) where
  imap = fmap

instance Applicative m => IxPointed (Ix m) where
  ireturn = pure

instance Applicative m => IxApplicative (Ix m) where
  iap = coerce $ (<*>) @m @a @b

data LinearState = LinearState {
    linearNextKey :: Nat,
    linearOpenKeys :: [Nat]
  }

newtype Linear s (i::LinearState) (j::LinearState) = Linear {unsafeRunLinear :: Ix IO i j a}
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)