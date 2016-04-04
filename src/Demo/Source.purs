-- imports

{-

import Demo.Env (Env, Rad)

type Id   = String
type Name = String

data MaxReq a = CheckId Id
              | GetAttr Id Name

check :: Id -> Rad Boolean
check id = dataFetch $ CheckId id

instance typeofMaxSource :: Typeable MaxRequest where
  typeof = TypeRep "MaxSource"

instance maxSource :: DataSource Env MaxRequest where
  fetch _ f env bfs = exec (unwrapState st) f env bfs

-}
