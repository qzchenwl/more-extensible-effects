{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Eff.Examples.Teletype where

import Control.Monad.Eff
import Control.Monad.Eff.Lift
import System.Exit (exitSuccess)

data Teletype x where
  GetLine     :: Teletype String
  PutStrLn    :: String -> Teletype ()
  ExitSuccess :: Teletype ()

putStrLn' :: Member Teletype r => String -> Eff r ()
putStrLn' = send . PutStrLn

getLine' :: Member Teletype r => Eff r String
getLine' = send GetLine

exitSuccess' :: Member Teletype r => Eff r ()
exitSuccess' = send ExitSuccess

runTeletype :: [String] -> Eff (Teletype ': r) a -> Eff r [String]
runTeletype ss = handleRelayS ss ret handle
  where
    ret :: [String] -> a -> Eff r [String]
    ret _ a = return []

    handle :: HandlerS [String] Teletype r [String]
    handle (s:stdin) GetLine      k = k stdin s
    handle _         GetLine      k = error "Insufficient input"
    handle stdin     (PutStrLn s) k = do
      stdout <- k stdin ()
      return (s:stdout)
    handle _         ExitSuccess  k = return []

runIOTeletype :: forall r a. MemberU2 Lift (Lift IO) r => Eff (Teletype ': r) a -> Eff r a
runIOTeletype = handleRelay ret handle
  where
    ret :: a -> Eff r a
    ret = return

    handle :: Handler Teletype r a
    handle GetLine      k = lift getLine      >>= k
    handle (PutStrLn s) k = lift (putStrLn s) >>= k
    handle ExitSuccess  k = lift exitSuccess  >>= k

example :: Member Teletype r => Eff r ()
example = do
  str <- getLine'
  putStrLn' ("put: " ++ str)
  str <- getLine'
  putStrLn' ("put: " ++ str)
  exitSuccess'
  putStrLn' "should not appear"
