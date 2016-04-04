module Rad.Core.DataCache
  ( DataCache
  , class Hashable
  , hash
  , empty
  , insert
  , lookup
  ) where

import Prelude (($), (<$>))

import Data.Exists ( Exists, mkExists, runExists )
import Data.Maybe  ( Maybe )
import Data.StrMap ( StrMap )
import Data.StrMap ( empty, insert, lookup )  as StrMap

import Unsafe.Coerce ( unsafeCoerce )

-- TODO: implement a 2 layer cache
newtype DataCache res = DataCache (StrMap (Exists res))

empty :: forall res. DataCache res
empty = DataCache StrMap.empty

insert :: forall res req a. (Hashable req a) => req a -> res a -> DataCache res -> DataCache res
insert req result (DataCache m) = DataCache $
  StrMap.insert (hash req) (mkExists result) m

-- So long as cache keys are globally unique
-- this coercion should be safe
lookup :: forall res req a. (Hashable req a) => req a -> DataCache res -> Maybe (res a)
lookup req (DataCache m) = runExists unsafeCoerce <$>
  StrMap.lookup (hash req) m

-- Hashable Class
class Hashable req a where
  hash :: req a -> String

