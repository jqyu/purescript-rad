module Rad.Core.Types.ResultVar
  ( SomeException
  , ResultVal(..)
  , ResultVar
  , new
  , empty
  , read
  , put
  , putSuccess
  , putFailure
  ) where

import Prelude (Unit, (<$>), (<<<))

import Control.Monad.Eff
import Control.Monad.Eff.Ref ( REF, Ref, newRef, readRef, writeRef )

import Data.Either (Either(Left,Right))

-- TODO: proper exception system
type SomeException = String

-- NOTE: Haxl uses MVars for this
-- Although AVars produce a similar result, they don't allow inspection of the "Done" state

data ResultVal a = ResultDone a
                 | ResultThrow SomeException
                 | ResultBlocked

newtype ResultVar a = ResultVar (Ref (ResultVal a))

new :: forall a e. a -> Eff ( ref :: REF | e ) (ResultVar a)
new x = ResultVar <$> newRef (ResultDone x)

empty :: forall a e. Eff ( ref :: REF | e ) (ResultVar a)
empty = ResultVar <$> newRef ResultBlocked

read :: forall a e. ResultVar a -> Eff ( ref :: REF | e ) (ResultVal a)
read (ResultVar a) = readRef a

put :: forall a e. ResultVar a -> ResultVal a -> Eff ( ref :: REF | e ) Unit
put (ResultVar a) = writeRef a

putSuccess :: forall a e. ResultVar a -> a -> Eff ( ref :: REF | e ) Unit
putSuccess (ResultVar a) = writeRef a <<< ResultDone

putFailure :: forall a e. ResultVar a -> SomeException -> Eff ( ref :: REF | e ) Unit
putFailure (ResultVar a) = writeRef a <<< ResultThrow
