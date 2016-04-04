{-

data Stage = Development
           | Environment
           | Production

type Env =
  { sessionId    :: Int
  , sessionToken :: String
  , stage        :: Stage
  }

type Rad = GenRad Env

-}
