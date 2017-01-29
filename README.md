# More Extensible Effects

This package is an implementation of ["Freer Monads, More Extensible Effects"](http://okmij.org/ftp/Haskell/extensible/more.pdf).

Much of the implementation is a repackaging and cleaning up of the reference materials provided [here](http://okmij.org/ftp/Haskell/extensible/):

- [Eff1.hs](http://okmij.org/ftp/Haskell/extensible/Eff1.hs)
- [OpenUnion5.hs](http://okmij.org/ftp/Haskell/extensible/OpenUnion5.hs)
- [FTCQueue1.hs](http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs)

## Overview

- Control
  - Monad
    - [Eff](src/Control/Monad/Eff.hs)
      - [Examples](src/Control/Monad/Eff/Examples.hs)
        - [Teletype](src/Control/Monad/Eff/Examples/Teletype.hs)
        - [VerboseAddition](src/Control/Monad/Eff/Examples/VerboseAddition.hs)
      - [Exception](src/Control/Monad/Eff/Exception.hs)
      - [Internal](src/Control/Monad/Eff/Internal.hs)
      - [Lift](src/Control/Monad/Eff/Lift.hs)
      - [NdetEff](src/Control/Monad/Eff/NdetEff.hs)
      - [Reader](src/Control/Monad/Eff/Reader.hs)
      - [State](src/Control/Monad/Eff/State.hs)
      - [StateRW](src/Control/Monad/Eff/StateRW.hs)
      - [Writer](src/Control/Monad/Eff/Writer.hs)
- Data
  - [FTCQueue](src/Data/FTCQueue.hs)
  - [OpenUnion](src/Data/OpenUnion.hs)

## Examples

### Log Effect ([24 Days of Hackage: extensible-effects](https://ocharles.org.uk/blog/posts/2013-12-04-24-days-of-hackage-extensible-effects.html))

[VerboseAddition.hs](src/Control/Monad/Eff/Examples/VerboseAddition.hs)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Eff.Examples.VerboseAddition where

import Control.Monad.Eff
import Control.Monad.Eff.Lift
import Prelude hiding (log)

-- | The Log Effect
data Log v where
  Log :: String -> Log ()

log :: Member Log r => String -> Eff r ()
log = send . Log

-- | The Log Interpreter, Pure
runLogger :: Eff (Log ': r) a -> Eff r (a, [String])
runLogger = handleRelay (\x -> return (x, []))
                        (\(Log s) k -> k () >>= \(x, ss) -> return (x, s:ss))

-- | The Log Interpreter, Impure
runIOLogger :: MemberU2 Lift (Lift IO) r => Eff (Log ': r) a -> Eff r a
runIOLogger = handleRelay return
                          (\(Log s) k -> lift (putStrLn s) >>= k)

-- | The program we want to be able to write
verboseAddition :: Member Log r => Eff r Int
verboseAddition = do
  log "I'm starting with 1..."
  x <- return 1

  log "and I'm adding 2..."
  y <- return 2

  let r = x + y

  log $ "Looks like the result is " ++ show r
  return r
```
Now we can run the program in pure or impure way:

```
λ> run (runLogger verboseAddition)
(3,["I'm starting with 1...","and I'm adding 2...","Looks like the result is 3"])
λ> runLift (runIOLogger verboseAddition)
I'm starting with 1...
and I'm adding 2...
Looks like the result is 3
3
```

### Teletype ([Purify code using free monads](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html))

[Teletype.hs](src/Control/Monad/Eff/Examples/Teletype.hs)

```haskell
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
```

Run it purely:

```
λ> run $ runTeletype ["hello", "world", "and more"] example
["put: hello","put: world"]

λ> run $ runTeletype ["hello"] example
*** Exception: Insufficient input
CallStack (from HasCallStack):
  error, called at /work/src/Control/Monad/Eff/Examples/Teletype.hs:35:39 in main:Control.Monad.Eff.Examples.Teletype
```

Run it in IO:

```
λ> runLift $ runIOTeletype example
hello
put: hello
world
put: world
*** Exception: ExitSuccess
```

## Usage Tips

### Effect Intepreter

The most complex part of new effect definition is the 'runX' function. As you can see in the above examples, it's usually defined by `handleRelay ret handle` with your customized `ret` to return value and `handle` to handle continuation.

It's similar to what you do to implement an instance of Monad (`ret` for `return`, `handle` for `>>=`). You can read `handleRelay ret handle` as run this monad with instance defined by `ret` and `handle`.

