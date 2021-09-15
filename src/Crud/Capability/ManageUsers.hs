module Crud.Capability.ManageUsers where

import Crud.User (User)
import Data.Text (Text)
import Data.UUID (UUID)

class Monad m => ManageUsers m where
  getUserByUuid :: UUID -> m (Maybe User)
  getUser :: Text -> m (Maybe User)
  putUser :: User -> m ()
  getAllUsers :: m [User]
