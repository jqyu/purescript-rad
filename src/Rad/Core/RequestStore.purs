module Rad.Core.RequestStore
  ( RequestStore
  , BlockedRequests(..)
  , empty
  , add
  , contents
  , BlockedRequestsExist
  , runBlockedRequests
  , applyBlockedRequests
  ) where

import Prelude (($), (<<<))
import Prelude (Unit, unit)

import Rad.Core.Types
  ( Flags
  , class DataSource
  , DataSourceName(..)
  , dataSourceName
  , BlockedFetches
  , BlockedFetch
  , PerformFetch(..)
  , class Request
  , hash
  , RequestExists
  , mkRequestExists
  )

import Data.StrMap ( StrMap )
import Data.StrMap ( empty, alter, values )  as StrMap
import Data.List (List(Nil), singleton, (:))
import Data.Maybe ( Maybe(..) )

import Unsafe.Coerce ( unsafeCoerce )

newtype RequestStore u = RequestStore (StrMap (BlockedRequestsExist u))

newtype BlockedRequests u req = BlockedRequests (BlockedFetches req)

empty :: forall u. RequestStore u
empty = RequestStore StrMap.empty

add :: forall u req a. (DataSource u req, Request req a)
    => BlockedFetch req a
    -> RequestStore u
    -> RequestStore u

add bf (RequestStore m) = RequestStore $
    StrMap.alter (Just <<< mkBlockedRequests <<< insert) (key name) m

  where insert :: Maybe (BlockedRequestsExist u) -> BlockedRequests u req
        insert Nothing = BlockedRequests $ singleton $ mkRequestExists bf
        insert (Just brs) = runBlockedRequests insertInto brs

        insertInto :: BlockedRequests u req -> BlockedRequests u req
        insertInto (BlockedRequests bfs) = BlockedRequests (mkRequestExists bf : unsafeCoerce bfs)

        name :: DataSourceName u (req a)
        name = dataSourceName
        key :: DataSourceName u (req a) -> String
        key (DataSourceName s) = s

contents :: forall u. RequestStore u -> List (BlockedRequestsExist u)
contents (RequestStore m) = StrMap.values m

-- UNSAFE existential functions
-- To ensure correctness, all sources must implement a UNIQUE source name
-- so that each list is of homogenous kind
foreign import data BlockedRequestsExist :: * -> *
foreign import mkBlockedRequests :: forall u req. (DataSource u req)
                                 => BlockedRequests u req
                                 -> BlockedRequestsExist u
foreign import runBlockedRequests :: forall u req r. (DataSource u req)
                                  => ( BlockedRequests u req -> r )
                                  -> BlockedRequestsExist u
                                  -> r
foreign import applyBlockedRequests :: forall eff u. Flags -> u
                                    -> BlockedRequestsExist u
                                    -> PerformFetch eff
