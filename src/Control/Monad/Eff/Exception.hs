{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Eff.Exception (
  Exception,
  throwException,
  runException,
  catchException
) where

import Control.Monad.Eff

-- | Exceptions of type e; no resumption
newtype Exception e v = Exception e

throwException :: Member (Exception e) r => e -> Eff r a
throwException = send . Exception

runException :: Eff (Exception e ': r) a -> Eff r (Either e a)
runException = handleRelay (return . Right) (\(Exception e) _ -> return (Left e))

-- | The handler is allowed to rethrow the exception
catchException :: Member (Exception e) r => Eff r a -> (e -> Eff r a) -> Eff r a
catchException m handle = interpose return (\(Exception e) k -> handle e) m

