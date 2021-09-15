module Crud.Api where

import Control.Monad (guard)
import Control.Monad.Time (MonadTime (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Crud.Capability.GenerateUUID (GenerateUUID (..))
import Crud.Capability.ManageUsers (ManageUsers (..))
import Crud.User (Register (..), User (..))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.UUID (UUID)
import Servant
import qualified Data.Text as T

type UserApi =
  Capture "uuid" UUID :> Get '[JSON] (Maybe User)
    :<|> Capture "username" Text :> Get '[JSON] (Maybe User)
    :<|> Get '[JSON] [User]
    :<|> ReqBody '[JSON] Register :> Post '[JSON] (Maybe User)

userByUuid :: ManageUsers m => UUID -> m (Maybe User)
userByUuid = getUserByUuid

userByName :: ManageUsers m => Text -> m (Maybe User)
userByName = getUser

allUsers :: ManageUsers m => m [User]
allUsers = getAllUsers

registerUser :: (GenerateUUID m, MonadTime m, ManageUsers m) => Register -> m (Maybe User)
registerUser reg = runMaybeT do
  let userName = reg_name reg
      userAge = reg_age reg
      userEmail = reg_email reg
  guard $ userAge > 12
  guard . not $ T.null userName
  newId <- lift genUuid
  time <- currentTime
  existing <- lift $ getUser userName
  guard $ isNothing existing
  let user =
        User
          { name = userName,
            uuid = newId,
            age = userAge,
            email = userEmail,
            registered = time
          }
  lift $ putUser user
  pure user

userServer :: (MonadTime m, GenerateUUID m, ManageUsers m) => ServerT UserApi m
userServer = getUserByUuid :<|> userByName :<|> allUsers :<|> registerUser
