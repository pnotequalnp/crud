module Crud.User where

import Control.Applicative
import Control.Category ((>>>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Email (Email)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.SQLite.Simple (FromRow (..), SQLData (..), ToRow (..), field)
import Database.SQLite.Simple.FromField (FromField (..), fieldData)
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)

data User = User
  { name :: Text,
    uuid :: UUID,
    age :: Int,
    email :: Email,
    registered :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance FromRow User where
  fromRow = User <$> field <*> (getUUID <$> field @UUID') <*> field <*> field <*> field

instance ToRow User where
  toRow User {..} = toRow (name, UUID' uuid, age, email, registered)

data Register = Register
  { reg_name :: Text,
    reg_age :: Int,
    reg_email :: Email
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UUID' = UUID' {getUUID :: UUID}

instance FromField UUID' where
  fromField =
    fieldData >>> \case
      SQLText t -> maybe empty (pure . UUID') $ UUID.fromText t
      _ -> empty

instance ToField UUID' where
  toField = SQLText . UUID.toText . getUUID

fromRegister :: UUID -> UTCTime -> Register -> User
fromRegister userUuid userRegistered Register {..} =
  User reg_name userUuid reg_age reg_email userRegistered
