# More Extensible Effects

This package is an implementation of ["Freer Monads, More Extensible Effects"](http://okmij.org/ftp/Haskell/extensible/more.pdf).

Much of the implementation is a repackaging and cleaning up of the reference materials provided [here](http://okmij.org/ftp/Haskell/extensible/):

- [Eff1.hs](http://okmij.org/ftp/Haskell/extensible/Eff1.hs)
- [OpenUnion5.hs](http://okmij.org/ftp/Haskell/extensible/OpenUnion5.hs)
- [FTCQueue1.hs](http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs)

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
