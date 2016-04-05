module Demo.Env
  ( DemoEnv(Production,Beta,Development)
  , Rad
  ) where

import Rad.Core (GenRad)

data DemoEnv = Production
             | Beta
             | Development

type Rad a = GenRad DemoEnv a
