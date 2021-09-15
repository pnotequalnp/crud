module Crud.Capability.GenerateUUID where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

class Monad m => GenerateUUID m where
  genUuid :: m UUID

instance GenerateUUID IO where
  genUuid = nextRandom
