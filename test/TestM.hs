module TestM where

import Control.Monad.State
import Control.Monad.Time
import Crud.Capability.GenerateUUID (GenerateUUID (..))
import Crud.Capability.ManageUsers (ManageUsers (..))
import Crud.User (User (..))
import Data.Foldable (find, toList)
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

data Database = Database
  { users :: Map Text User,
    uuidQueue :: [UUID],
    timeQueue :: [UTCTime]
  }
  deriving stock (Eq, Show)

newtype TestM a = TestM (State Database a)
  deriving newtype (Functor, Applicative, Monad, MonadState Database)

instance GenerateUUID TestM where
  genUuid = do
    q <- gets uuidQueue
    case q of
      [] -> error "Not enough UUIDs in queue"
      next : rest -> next <$ modify (\db -> db {uuidQueue = rest})

instance MonadTime TestM where
  currentTime = do
    q <- gets timeQueue
    case q of
      [] -> error "Not enough timestamps in queue"
      next : rest -> next <$ modify (\db -> db {timeQueue = rest})

instance ManageUsers TestM where
  getUserByUuid userUuid =
    gets users <&> find ((== userUuid) . uuid)

  getUser userName =
    gets users <&> M.lookup userName

  putUser u = do
    us <- gets users
    let us' = M.insert (name u) u us
    modify \db -> db {users = us'}

  getAllUsers =
    gets users <&> toList

runTestM :: Database -> TestM a -> (a, Database)
runTestM db (TestM t) = runState t db
