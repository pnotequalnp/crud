module Test where

import Control.Monad (replicateM)
import Crud.Api
import Crud.User (Register (..), User (..), fromRegister)
import Data.Email (Email (..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day (ModifiedJulianDay), UTCTime (..), picosecondsToDiffTime)
import Data.UUID (UUID)
import Data.UUID.Types.Internal (buildFromBytes)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import TestM

genUser :: Gen User
genUser = do
  userName <- Gen.text (Range.linear 1 64) Gen.unicode
  userUuid <- genUuid
  userAge <- Gen.int $ Range.constant 13 200
  userEmail <- Email . T.intercalate "@" <$> replicateM 2 (Gen.text (Range.linear 1 128) Gen.unicode)
  userRegistered <- genTime
  pure $ User userName userUuid userAge userEmail userRegistered

genUserMap :: Range Int -> Gen (Map Text User)
genUserMap r = go <$> Gen.list r genUser
  where
    go us = M.fromList $ zip (name <$> us) us

genUuid :: Gen UUID
genUuid = do
  [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf] <- replicateM 16 $ Gen.word8 Range.constantBounded
  pure $ buildFromBytes 4 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf

genTime :: Gen UTCTime
genTime = do
  day <- ModifiedJulianDay <$> Gen.integral (Range.linearFrom 59215 0 88069)
  diffTime <- picosecondsToDiffTime <$> Gen.integral (Range.constant 0 (86400 * 10 ^ (12 :: Int)))
  pure $ UTCTime day diffTime

genRegister :: Gen Register
genRegister = do
  User {name, age, email} <- genUser
  pure $ Register name age email

hprop_register :: Property
hprop_register = property do
  reg <- forAll genRegister
  users <- forAll $ M.delete (reg_name reg) <$> genUserMap (Range.constant 0 12)
  userUuid <- forAll genUuid
  userRegistered <- forAll genTime
  let db = Database users [userUuid] [userRegistered]
      (user, db') = runTestM db $ registerUser reg
      expectedUser = fromRegister userUuid userRegistered reg
      expectedDb = Database users' [] []
      users' = M.insert (name expectedUser) expectedUser users

  user === Just expectedUser
  db' === expectedDb
