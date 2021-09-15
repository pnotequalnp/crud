module Data.Email where

import Control.Applicative
import Control.Category ((>>>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..), fieldData)
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)

newtype Email = Email Text
  deriving stock (Eq, Generic)
  deriving newtype (Show, ToJSON, FromJSON)

parseEmail :: Text -> Maybe Email
parseEmail =
  T.splitOn "@" >>> \case
    [prefix, domain] -> Just . Email $ prefix <> "@" <> domain
    _ -> Nothing

instance FromField Email where
  fromField =
    fieldData >>> \case
      SQLText t -> maybe empty pure $ parseEmail t
      _ -> empty

instance ToField Email where
  toField (Email e) = SQLText e
