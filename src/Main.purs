module Main where

-- import Prelude (Unit, unit)
import Prelude hiding (add)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Traversable
import Rad.Core.DataCache (class Hashable)
import Rad.Core.RequestStore
import Rad.Core.Types
import Rad.Core.Types.ResultVar as ResultVar

data Foo a = Foo a

instance bar :: Hashable Foo String where
  hash (Foo s) = s

instance foo :: DataSource Unit Foo where
  dataSourceName = DataSourceName "test"
  fetch _ _ _ = SyncFetch $ log "yes halo"

store :: RequestStore Unit
store = empty

makeReq :: forall e. String -> RequestStore Unit -> RadEff e (RequestStore Unit)
makeReq s st = do
  rvar <- ResultVar.empty
  pure $ add (BlockedFetch { req: Foo s, rvar: rvar }) st

main :: forall e. RadEff e Unit
main = do
  log "Hello sailor!"
  s' <- makeReq "test" store
  s'' <- makeReq "test2" store
  let l = contents s''
      m = applyBlockedRequests defaultFlags unit <$> l
      unwrap (SyncFetch x) = x
      unwrap (AsyncFetch _) = pure unit
  traverse unwrap m
  log "Test"
