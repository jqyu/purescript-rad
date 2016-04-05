-- TODOS:
--   non-empty environment initialization
--   request replaying
--   memoization

module Rad.Core.Monad
  ( Env(..)
  , emptyEnv
  , Result
  , GenRad(..)
  , runRad
  , throwRad
  , dataFetch
  , uncachedRequest
  ) where

import Prelude
  ( Unit
  , class Applicative
  , class Apply
  , class Bind
  , class Functor
  , class Show
  , unit
  , pure
  , bind
  , map
  , show
  , ($)
  , (+)
  , (<>)
  , (<$>)
  , (<*>)
  , (>>=)
  , (<<<)
  )

import Control.Alt ( class Alt )
import Control.Bind ( (>=>) )
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Par (runPar, Par(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception ( EXCEPTION, throw )
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, modifyRef, writeRef)
import Control.Plus ( class Plus )

import Data.Either (Either(Left,Right))
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(Just,Nothing))
import Data.List (List, length)
import Data.Traversable (traverse)

import Rad.Core.Types
  ( Flags
  , defaultFlags
  , Stats
  , emptyStats

  , RadAff
  , RadEff
  , class DataSource
  , fetch
  , BlockedFetches
  , BlockedFetch(..)
  , PerformFetch(..)
  , class Request
  , hash

  , SomeException
  , ResultVal(..)
  , ResultVar
  )

import Rad.Core.DataCache (DataCache)
import Rad.Core.DataCache (empty, insert, lookup) as DataCache
import Rad.Core.RequestStore (RequestStore, BlockedRequests(..), BlockedRequestsExist, applyBlockedRequests)
import Rad.Core.RequestStore (empty, add, contents) as RequestStore
import Rad.Core.Types.ResultVar (empty, read) as ResultVar


------------------------------------
-- | ENVIRONMENT
--
-- A Rad monad describes a set of instructions on how to produce the desired response.
-- In order to perform these instructions, we must pass data throughout the computation
-- that describes aspects of our environment

newtype Env u = Env
  { cacheRef :: Ref (DataCache ResultVar)
-- TODO: implement memoization
--  , memoRef  :: Ref (DataCache (MemoVar u))
  , flags    :: Flags
  , userEnv  :: u
  , statsRef :: Ref Stats
  }

-- TODO: allow loading environments to "replay" them
emptyEnv :: forall eff u. u -> Eff ( ref :: REF | eff ) (Env u)
emptyEnv e = do
  cref <- newRef DataCache.empty
--   mref <- newRef DataCache.empty
  sref <- newRef emptyStats
  pure $ Env
    { cacheRef : cref
--     , memoRef  : mref
    , flags    : defaultFlags
    , userEnv  : e
    , statsRef : sref
    }


------------------------------------
-- | THE RAD MONAD
--
-- It does fucking everything.
-- It acts as a reader, IO, contains an applicative combinator for efficient batching...

newtype GenRad u a = GenRad forall eff
   . Env u
  -> Ref (RequestStore u)
  -> RadEff eff (Result u a)

unRad :: forall u a eff
   . GenRad u a
  -> Env u
  -> Ref (RequestStore u)
  -> RadEff eff (Result u a)
unRad (GenRad f) = f


-- | The result of a computation is either
-- "Done" with a value
-- "Throw" with an exception or
-- "Blocked" on some data fetch and a continuation*
-- if you're not familiar with FP, continuations are a generalization of callbacks/promises
throwRad :: forall u a. SomeException -> GenRad u a
throwRad e = GenRad \_env _ref -> pure $ Throw e

data Result u a = Done a
                | Throw SomeException
                | Blocked (Cont u a)

data Cont u a = Cont (GenRad u a)
              | ContBind  (Exists (BindCont  u a)) -- Cont u b        :>>= (b -> GenRad u a)
              | ContApply (Exists (ApplyCont u a)) -- Cont u (b -> a) :<*> Cont u b
              | ContMap   (Exists (MapCont   u a)) --        (b -> a) :<$> Cont u b

data BindCont  u a b = BindCont  (Cont u b)        (b -> GenRad u a)
data ApplyCont u a b = ApplyCont (Cont u (b -> a)) (Cont u b)
data MapCont   u a b = MapCont           (b -> a)  (Cont u b)

-- syntactic sugar for constructors, unfortunately we can't apply pattern matching to these
infixl 1 bindCont as :>>=
infixl 4 applyCont as :<*>
infixl 4 mapCont as :<$>

bindCont  :: forall u a b. Cont u b -> (b -> GenRad u a) -> Cont u a
bindCont  m k = ContBind  $ mkExists $ BindCont  m k
applyCont :: forall u a b. Cont u (b -> a) -> Cont u b -> Cont u a
applyCont f x = ContApply $ mkExists $ ApplyCont f x
mapCont   :: forall u a b. (b -> a) -> (Cont u b) -> Cont u a
mapCont   f x = ContMap   $ mkExists $ MapCont   f x

-- see https://github.com/facebook/Haxl/blob/master/Haxl/Core/Monad.hs
-- this implementation is pretty messy due to language limitations
toRad :: forall u a. Cont u a -> GenRad u a
toRad (Cont rad) = rad
toRad (ContBind cont) = runExists to cont                                      -- :>>=
  where to :: forall b. BindCont u a b -> GenRad u a
        to (BindCont (ContBind c') k) = runExists (comp k) c'
        to (BindCont c             k) = toRad c >>= k
        comp :: forall b c. (b -> GenRad u a) -> (BindCont u b c) -> GenRad u a
        comp k2 (BindCont m k1) = toRad (m :>>= (k1 >=> k2))
toRad (ContApply cont) = runExists to cont                                     -- :<*>
  where to :: forall b. ApplyCont u a b -> GenRad u a
        to (ApplyCont (ContMap c') (ContMap c'')) = runExists (runExists reassoc c') c''
        to (ApplyCont f            x            ) = toRad f <*> toRad x
        reassoc :: forall b c d. MapCont u (b -> a) c -> MapCont u b d -> GenRad u a
        reassoc (MapCont f i) (MapCont g j) = toRad ((\x y -> f x (g y)) :<$> i :<*> j)
toRad (ContMap cont) = runExists to cont
  where to :: forall b. MapCont u a b -> GenRad u a
        to (MapCont f (ContMap c')) = runExists (comp f) c'
        to (MapCont f x) = f <$> toRad x
        comp :: forall b c. (b -> a) -> MapCont u b c -> GenRad u a
        comp f (MapCont g x) = toRad ((f <<< g) :<$> x)

--------------------------------------------
-- | Rad Monad Instances

instance showResult :: (Show a) => Show (Result u a) where
  show (Done a) = "Done(" <> show a <> ")"
  show (Throw e) = "Throw(" <> show e <> ")"
  show (Blocked _) = "Blocked"

-- >>=
instance bindRad :: Bind (GenRad u) where
  bind (GenRad m) k = GenRad \env ref -> do
    r <- m env ref
    case r of
         Done a     -> unRad (k a) env ref
         Throw e    -> pure $ Throw    e
         Blocked k' -> pure $ Blocked (k' :>>= k)

-- <$>
instance mapRad :: Functor (GenRad u) where
  map f (GenRad m) = GenRad \env ref -> do
    r <- m env ref
    case r of
         Done a     -> pure $ Done    (f a)
         Throw e    -> pure $ Throw    e
         Blocked a' -> pure $ Blocked (f :<$> a')
-- <*>
instance applyRad :: Apply (GenRad u) where
  apply (GenRad f) (GenRad a) = GenRad \env ref -> do
    r <- f env ref
    case r of
         Throw e -> pure $ Throw e
         Done f' -> do
           ra <- a env ref
           case ra of 
                Done a'    -> pure $ Done    (f' a')
                Throw e    -> pure $ Throw    e
                Blocked a' -> pure $ Blocked (f' :<$> a')
         Blocked f' -> do
           ra <- a env ref
           case ra of
                Done a'    -> pure $ Blocked (($ a') :<$> f')
                Throw e    -> pure $ Blocked (f' :<*> Cont (throwRad e))
                Blocked a' -> pure $ Blocked (f' :<*> a')

-- pure
instance applicativeRad :: Applicative (GenRad u) where
  pure a = GenRad \_env _ref -> pure $ Done a

-- JQYU came up with these all on his own [:
-- (which means they probably don't work)

-- <|>
instance altRad :: Alt (GenRad u) where
  alt (GenRad l) (GenRad r) = GenRad \env ref -> do
    l' <- l env ref
    case l' of
         Done a     -> pure $ Done a
         Throw e    -> r env ref
         Blocked a' -> pure $ Throw "TODO"

--- empty
instance plusRad :: Plus (GenRad u) where
  empty = GenRad \_env _rev -> pure $ Throw "UNDEFINED: generated by Plus instance"

-- | Run a Rad computation w.r.t. an Env
-- TODO: switch to faster version if no EventLog is needed
runRad :: forall u a eff. Env u -> GenRad u a -> RadAff eff a
runRad env h = do
  let go :: Int -> Env u -> Cont u a -> RadAff eff a
      go n env' c = do
        traceEventIO "START COMPUTATION"
        -- create an empty request store for the current layer of computation
        ref <- liftEff $ newRef RequestStore.empty
        -- perform computation with the side effect of populating the request store
        e <- liftEff $ unRad (toRad c) env' ref
        traceEventIO "STOP COMPUTATION"
        case e of
             Done a     -> pure a              -- Nothing is blocked, move on
             Throw err  -> liftEff $ throw err -- Throws an exception
             Blocked c' -> do                  -- blocked, eval another round
               bs <- liftEff $ readRef ref
               liftEff $ writeRef ref RequestStore.empty
               traceEventIO "START performFetches"
               n' <- performFetches n env' bs
               traceEventIO "STOP performFetches"
               go n' env' c'
  traceEventIO "START runRad"
  r <- go 0 env (Cont h)
  traceEventIO "STOP runRad"
  pure r


--------------------------------------------
-- | Data Fetching and Caching

-- | Possible responses when checking the cache
data CacheResult a = Uncached          (ResultVar a)
                   | CachedNotFetched  (ResultVar a)
                   | Cached (Either SomeException a)

cached :: forall u req a eff. (DataSource u req, Request req a)
       => Env u -> req a -> RadEff eff (CacheResult a)
cached (Env env) req = do
  cache <- readRef env.cacheRef
  -- purescript is evaluated strictly
  let do_fetch :: Unit -> RadEff eff (CacheResult a)
      do_fetch _ = do
        rvar <- ResultVar.empty
        writeRef env.cacheRef $ DataCache.insert req rvar cache
        pure (Uncached rvar)
  case DataCache.lookup req cache of
       Nothing -> do_fetch unit
       Just rvar -> do
         mb <- ResultVar.read rvar
         case mb of
              ResultBlocked -> pure $ CachedNotFetched rvar
              ResultThrow e -> pure $ Cached (Left e)
              ResultDone a  -> pure $ Cached (Right a)

-- | Performs actual fetching of data for a 'Request' from a 'DataSource'
dataFetch :: forall u req a. (DataSource u req, Request req a) => req a -> GenRad u a
dataFetch req = GenRad \env ref -> do
  -- First, check the cache
  res <- cached env req
  case res of 
       -- Not seem before: add the request to the RequestStore, so it
       -- will be fetched in the next round.
       Uncached rvar -> do
         modifyRef ref $ RequestStore.add $ BlockedFetch req rvar
         pure $ Blocked $ Cont $ continueFetch req rvar
       CachedNotFetched rvar ->
         pure $ Blocked $ Cont $ continueFetch req rvar
       Cached (Left  e) -> pure $ Throw e
       Cached (Right a) -> pure $ Done  a

-- | A data request that is not cached. this is not what you want for
-- normal read requests, because then multiple identical requests may
-- return different results, and this invalidates some of the properties
-- that we expect Rad computations to respect...
-- useful for performing writes

uncachedRequest :: forall u req a. (DataSource u req, Request req a)
                => req a -> GenRad u a
uncachedRequest req = GenRad \_env ref -> do
  rvar <- ResultVar.empty
  modifyRef ref $ RequestStore.add $ BlockedFetch req rvar
  pure $ Blocked $ Cont $ continueFetch req rvar

-- creates a continuation reflecting a blocked value to be retrieved in a current fetch
continueFetch :: forall u req a. (DataSource u req)
              => req a -> ResultVar a -> GenRad u a
continueFetch req rvar = GenRad \_env _ref -> do
  m <- ResultVar.read rvar
  case m of
       ResultDone a  -> pure $ Done a
       ResultThrow e -> pure $ Throw e
       ResultBlocked -> pure $ Throw "result wasn't fetched or thrown..."

-- TODO: cacheResult 

-- TODO: cacheRequest, for replaying requests

performFetches :: forall u eff. Int -> Env u -> RequestStore u -> RadAff eff Int
performFetches n (Env env) reqs = do
  let sref = env.statsRef
      jobs :: List (BlockedRequestsExist u)
      jobs = RequestStore.contents reqs
      n'   = n + length jobs
  -- TODO: stats
  runPar $ traverse (toPar <<< applyBlockedRequests env.flags env.userEnv) jobs
  pure n'

toPar :: forall eff. PerformFetch eff -> Par ( console :: CONSOLE, ref :: REF, err :: EXCEPTION | eff ) Unit
toPar  (SyncFetch x) = Par $ liftEff x
toPar (AsyncFetch x) = Par x

-- TODO: see if there's an idiomatic purescript way of tracing event IO
traceEventIO :: forall eff. String -> RadAff eff Unit
traceEventIO = log
