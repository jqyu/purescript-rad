module Rad.Core.Types.Request
  ( class Request
  , hash
  , RequestExists
  , mkRequestExists
  , runRequestExists
  ) where

class Request req a where
  hash :: req a -> String

foreign import data RequestExists :: ((* -> *) -> * -> *) -> (* -> *) -> *

foreign import mkRequestExists :: forall f req a
   . (Request req a)
  => f req a
  -> RequestExists f req

foreign import runRequestExists :: forall f req r
   . ( forall a. (Request req a)
         => f req a
         -> r
     )
  -> RequestExists f req
  -> r
