module Rad.Core.Types.Debug

  ( Flags(..)
  , defaultFlags
  , ifTrace
  , ifReport
  , Stats
  , emptyStats
  ) where

import Prelude (Unit, class Monad, (>=))
import Control.Monad (when)

import Data.List (List(Nil))

-- | Flags that control the operation of the engine

newtype Flags = Flags
  { trace  :: Int
  , report :: Int
  }

defaultFlags :: Flags
defaultFlags = Flags
  { trace  : 0
  , report : 1
  }

-- TODO: void the result so this can be done forall a. m a
-- | Runs an action if the tracing level is above the given threshold
ifTrace :: forall m. (Monad m) => Flags -> Int -> m Unit -> m Unit
ifTrace (Flags f) i = when (f.trace >= i)

-- | Runs an action if the report level is above a given threshold
ifReport :: forall m. (Monad m) => Flags -> Int -> m Unit -> m Unit
ifReport (Flags f) i = when (f.report >= i)

-- | TODO: stats

type Microseconds = Int

newtype Stats = Stats (List RoundStats)

emptyStats :: Stats
emptyStats = Stats Nil

newtype RoundStats = RoundStats
  { roundTime :: Microseconds
  -- TODO: add more stats types
  }

