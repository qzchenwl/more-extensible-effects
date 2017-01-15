{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Eff.Lift (
  Lift,
  lift,
  runLift
) where

import Control.Monad.Eff
import Control.Monad.Eff.Internal
import Data.OpenUnion

-- | Lifting: emulating monad transformers
newtype Lift m a = Lift (m a)

lift :: (MemberU2 Lift (Lift m) r) => m a -> Eff r a
lift = send . Lift

runLift :: Monad m => Eff '[Lift m] w -> m w
runLift (Pure x) = return x
runLift (Impure u q) = case prj u of
                         Just (Lift m) -> m >>= runLift . qApp q
                         -- Nothing cannot occur
