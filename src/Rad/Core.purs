module Rad.Core
  ( module Rad.Core.Monad
  , module Rad.Core.Types
  , module Rad.Core.Types.ResultVar
  ) where

import Rad.Core.Monad
  ( Env(..)
  , emptyEnv
  , GenRad(..)
  , runRad
  , throwRad
  , dataFetch
  , uncachedRequest
  )

import Rad.Core.Types
  ( Flags
  , RadEff
  , RadAff
  -- * DataSource implementation
  , class DataSource
  , DataSourceName(..)
  , dataSourceName
  , fetch
  , PerformFetch(..)
  , BlockedFetches
  , BlockedFetch(..)
  , class Request
  , hash
  , RequestExists
  , mkRequestExists
  , runRequestExists
  -- * ResultVar implementation
  , SomeException
  , ResultVal(..)
  , ResultVar
  )

import Rad.Core.Types.ResultVar
  ( put
  , putSuccess
  , putFailure
  )

