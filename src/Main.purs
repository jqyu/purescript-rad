module Main where

-- import Prelude (Unit, unit)
import Prelude hiding (add)
import Control.Alt
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff (runAff)

import Data.GraphQL (toAST)
import Data.Traversable

import Rad.Core
import Rad.Core.RequestStore
import Rad.Core.Types
import Rad.Core.Types.ResultVar as ResultVar

foreign import timerStart :: forall e. String -> Eff (console :: CONSOLE | e) Unit
foreign import timerEnd   :: forall e. String -> Eff (console :: CONSOLE | e) Unit

import Demo.Source.Echo
import Demo.Env

main' :: forall e. RadEff e Unit
main' = do
  log "Hello sailor!"
  timerStart "req perf"
  s' <- makeReq "test" store
  s'' <- makeReq "test2" store
  let l = contents s''
      m = applyBlockedRequests defaultFlags unit <$> l
      unwrap (SyncFetch x) = x
      unwrap (AsyncFetch _) = pure unit
  traverse unwrap m
  timerEnd "req perf"
  timerStart "parse perf"
  log $ show $ toAST kitchenSink
  timerEnd "parse perf"

nnull :: Rad Int
nnull = throwRad "uh the fuck oh!!"
    <|> echo 5

n1 :: Rad Int
n1 = echo 1

n2 :: Rad Int
n2 = echo 2

n3 :: Rad Int
n3 = add4 <$> n1 <*> n1 <*> n2 <*> nnull

n4 :: Rad Int
n4 = add4 <$> n3 <*> n1 <*> n2 <*> n3

add4 :: Int -> Int -> Int -> Int -> Int
add4 a b c d = a + b + c + d



n5 :: Rad Int
n5 = add4 <$> n1 <*> n2 <*> n3 <*> n4

main :: forall e. RadEff e Unit
main = do
  main'
  timerStart "TEST"
  runAff (\err -> log $ show err) (\_ -> timerEnd "TEST") $ do
    env <- liftEff $ emptyEnv Production
    x <- runRad env n5
    liftEff $ log $ show x
    

query :: String
query = "{"
     <> "  field {"
     <> "    myFoo: foo"
     <> "    bar"
     <> "  }"
     <> "}"

tq :: String 
tq = ",, query _nA1_Me { a: f } ,,,,,,,          \n   mutation query { b: f } query a { c: f }"

kitchenSink :: String
kitchenSink =
     "query myQuery($foo: Test) {"
  <> "  foo\n\n"
  <> "  ... on myFragment @skip (if: true) { test }"
  <> "  ... test"
  <> "  testField {"
  <> "    ... on MyType {"
  <> "      myField(withArg: \"string literal\")" -- TODO: fix this
  <> "      myField(withArg: \"STRING LITERAL !!!!!!!\")" -- TODO: fix this
  <> "      myField(withArg: \"string literal \\\"with escaping \\\" \\\\ \n\n\")" -- TODO: fix this
  <> "    }"
  <> "  }"
  <> "}"
  <> "query otherQuery($foo: Test) {"
  <> "  myFun(int: 1234)"
  <> "  myFun(int: -1234)"
  <> "  myFun(frac: 1042.4)"
  <> "  thing"
  <> "  two(frac: 1041.12, exp: 1042E-142, both: 5356.14e1)"
  <> "}"
  <> "fragment myFragment on User {"
  <> "  my, fields, with(args: $test, arg2: MYENUM, arg3: [ MYENUM, MYENUM, MYENUM ])"
  <> "}"

-- THING

data Foo a = Foo a

instance bar :: Request Foo String where
  hash (Foo s) = s

instance foo :: DataSource Unit Foo where
  dataSourceName = DataSourceName "test"
  fetch _ _ _ = SyncFetch $ log "yes halo"

store :: RequestStore Unit
store = empty

makeReq :: forall e. String -> RequestStore Unit -> RadEff e (RequestStore Unit)
makeReq s st = do
  rvar <- ResultVar.empty
  pure $ add (BlockedFetch (Foo s) rvar) st

