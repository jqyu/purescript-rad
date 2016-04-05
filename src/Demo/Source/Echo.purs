module Demo.Source.Echo
  ( Echoer
  , echo
  ) where


import Prelude (Unit, class Show, show, bind, pure, unit, ($), (<>))

import Control.Monad.Eff.Console (log)

import Data.Traversable (traverse)

import Demo.Env (DemoEnv(..), Rad)

import Rad.Core
  ( RadEff
  , class Request
  , hash
  , class DataSource
  , DataSourceName(..)
  , PerformFetch(..)
  , dataFetch
  , BlockedFetch(..)
  , runRequestExists
  , putSuccess
  )

data Echoer a = Echo a

instance echoerDataSource :: DataSource DemoEnv Echoer where

  dataSourceName = DataSourceName "Echoer"

  fetch _ _ bfs = SyncFetch $ do
    let exec :: forall a eff
              . BlockedFetch Echoer a
             -> RadEff eff Unit
        exec (BlockedFetch req@(Echo a) rvar) = do
          putSuccess rvar a
    traverse (runRequestExists exec) bfs
    pure unit

data Novel = Novel

instance hashEchoerNovel :: Request Echoer Novel where
  hash (Echo Novel) = "Echo::NOVEL"

instance hashEchoerShow :: (Show a) => Request Echoer a where
  hash (Echo a) = "Echo::" <> show a

echo :: forall a. (Show a) => a -> Rad a
echo a = dataFetch $ Echo a
