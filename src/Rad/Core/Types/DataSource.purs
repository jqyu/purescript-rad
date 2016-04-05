module Rad.Core.Types.DataSource
  ( RadEff
  , RadAff
  , class DataSource
  , DataSourceName(..)
  , dataSourceName
  , fetch
  , PerformFetch(..)
  , BlockedFetches
  , BlockedFetch(..)
  ) where

import Prelude (Unit)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception ( EXCEPTION )

import Data.Exists (Exists)
import Data.List (List(Nil))

import Rad.Core.Types.Debug (Flags)
import Rad.Core.Types.ResultVar (ResultVar)
import Rad.Core.Types.Request (RequestExists)

-- | Fetch side effects

type RadEff eff a = Eff ( err :: EXCEPTION, console :: CONSOLE, ref :: REF, avar :: AVAR | eff ) a
type RadAff eff a = Aff ( err :: EXCEPTION, console :: CONSOLE, ref :: REF, avar :: AVAR | eff ) a

-- | Data source

newtype DataSourceName u r = DataSourceName String

class DataSource u req where
  dataSourceName :: forall a. DataSourceName u (req a)
  fetch :: forall eff. Flags -> u -> BlockedFetches req -> PerformFetch eff

-- TODO: in Haxl, an AsyncFetch has the signature ( IO () -> IO () )
-- the "a" phantom type is used to resolve to a class instance
-- in theory this can be used to restrict return values
data PerformFetch eff =  SyncFetch (RadEff eff Unit)
                      | AsyncFetch (RadAff eff Unit)

type BlockedFetches req = List (RequestExists BlockedFetch req)
data BlockedFetch req a = BlockedFetch (req a) (ResultVar a)
