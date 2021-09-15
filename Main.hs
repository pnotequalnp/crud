module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Time
import Crud.Api (UserApi, userServer)
import Crud.Capability.GenerateUUID (GenerateUUID (..))
import Crud.Capability.ManageUsers (ManageUsers (..))
import Crud.User (UUID' (..))
import Database.SQLite.Simple
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (getEnv)

main :: IO ()
main = do
  port <- read <$> getEnv "CRUD_PORT"
  conn <- getEnv "CRUD_DB_FILE" >>= open
  execute_ conn "CREATE TABLE IF NOT EXISTS users (name TEXT UNIQUE NOT NULL, uuid TEXT UNIQUE NOT NULL, age INTEGER NOT NULL, email TEXT NOT NULL, registered TEXT NOT NULL, PRIMARY KEY(name, uuid))"
  let app = serve (Proxy @UserApi) $ server conn
  run port app

newtype AppServer a = AppServer (ReaderT Connection Handler a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Connection)

runAppServer :: Connection -> AppServer a -> Handler a
runAppServer conn (AppServer s) = runReaderT s conn

instance MonadTime AppServer where
  currentTime = liftIO currentTime

instance GenerateUUID AppServer where
  genUuid = liftIO genUuid

instance ManageUsers AppServer where
  getUserByUuid uuid = do
    conn <- ask
    res <- liftIO . query conn "SELECT * FROM users WHERE uuid = (?)" . Only $ UUID' uuid
    case res of
      [user] -> pure $ Just user
      _ -> pure Nothing

  getUser username = do
    conn <- ask
    res <- liftIO . query conn "SELECT * FROM users WHERE name = (?)" $ Only username
    case res of
      [user] -> pure $ Just user
      _ -> pure Nothing

  putUser user = do
    conn <- ask
    liftIO . execute conn "INSERT INTO users (name, uuid, age, email, registered) VALUES (?,?,?,?,?)" $ user

  getAllUsers = do
    conn <- ask
    liftIO $ query_ conn "SELECT * FROM users"

server :: Connection -> Server UserApi
server conn = hoistServer (Proxy @UserApi) (runAppServer conn) userServer
