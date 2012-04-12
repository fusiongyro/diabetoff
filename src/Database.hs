{- module Database ( setupDatabase
                , recordWeighIn
                , lastWeeksLoss
                , authenticate
                , createUser
                , updateTargetWeight
                , allWeighIns
                , targetWeight ) where -}
module Database ( module Database
                , Connection(..)) where

import Control.Applicative
import Data.Time.Calendar

import Database.HDBC
import Database.HDBC.PostgreSQL

import Types

connect = connectPostgreSQL

disconnect :: IConnection conn => conn -> IO ()
disconnect = Database.HDBC.disconnect

doubleToFloat :: Double -> Float
doubleToFloat = convertFloat

convertFloat :: (RealFloat a, RealFloat c) => a -> c
convertFloat = uncurry encodeFloat . decodeFloat

-- | Save a weigh-in for a particular user
recordWeighIn :: (IConnection c) => c -> Name -> Weight -> Day -> IO ()
recordWeighIn dbh name weight day = do
  run dbh 
      "INSERT INTO weighins (name, weight, measured_on) \
      \VALUES (?, ?, ?)"
      [toSql name, toSql weight, toSql day]
  return ()

lastWeeksLoss :: (IConnection c) => c -> IO [(Name, Float)]
lastWeeksLoss db = map convert <$> quickQuery' db query []
  where
    query = "SELECT * FROM last_weeks_loss"
    convert [name, loss] = (fromSql name, doubleToFloat $ fromSql loss)

authenticate :: (IConnection c) => c -> Name -> Password -> IO Bool
authenticate db name password = 
  (fromSql . head . head) <$> quickQuery' db "SELECT authenticate(?, ?)" [toSql name, toSql password]

createUser :: (IConnection c) => c -> Name -> Password -> IO Bool
createUser db name password = do
  run db "SELECT create_user(?, ?)" [toSql name, toSql password]
  return True

updateTargetWeight :: (IConnection c) => c -> Name -> Weight -> IO ()
updateTargetWeight db name weight = do
  run db "UPDATE users SET target_weight = ? WHERE name = ?" [toSql weight, toSql name]
  return ()

-- | Ensure the database is properly configured for this version
setupDatabase :: (IConnection c) => c -> IO ()
setupDatabase dbh = do
  version <- schemaVersion dbh
  upgradeSchema version dbh
  commit dbh

-- | Return the current version of the schema by inspecting the schema_version 
-- table
schemaVersion :: (IConnection c) => c -> IO Integer
schemaVersion dbh = do
  tables <- getTables dbh
  version <- if "schema_version" `elem` tables
    then (fromSql . head . head) <$> quickQuery' dbh versionQuery []
    else return 0
  return version
    where
      versionQuery = "SELECT version FROM schema_version"

-- | Return all the weigh-ins for this user
allWeighIns :: (IConnection c) => c -> Name -> IO [(Day, Float)]
allWeighIns dbh name = do
  map convert <$> quickQuery' dbh query [toSql name]
    where
      query = "SELECT measured_on, weight FROM weighins WHERE name = ?"
      convert [day, weight] = (fromSql day, doubleToFloat $ fromSql weight)

targetWeight :: (IConnection c) => c -> Name -> IO Weight
targetWeight dbh name = 
  convert <$> quickQuery' dbh query [toSql name]
  where
    query = "SELECT target_weight FROM users WHERE name = ?"
    
    convert []          = 0
    convert [[SqlNull]] = 0
    convert [[x]]       = fromSql x

-- | Upgrade the database schema to the current version, if necessary
upgradeSchema 0 dbh = do
  runRaw dbh 
         "CREATE EXTENSION pgcrypto;\n\
         \\n\
         \CREATE TABLE users (\n\
         \  name VARCHAR PRIMARY KEY,\n\
         \  password CHAR(60) NOT NULL,\n\
         \  pwsalt CHAR(29) NOT NULL,\n\
         \  target_weight INTEGER\n\
         \);\n\
         \\n\
         \CREATE FUNCTION create_user(name VARCHAR, password VARCHAR) \
         \RETURNS VARCHAR AS $$\n\
         \  INSERT INTO users (name, password, pwsalt)\n\
         \  SELECT $1 AS name, crypt($2, salt), salt\n\
         \  FROM gen_salt('bf') AS salt\n\
         \  RETURNING name\n\
         \$$ LANGUAGE SQL;\n\
         \\n\
         \CREATE FUNCTION authenticate(name VARCHAR, password VARCHAR) \
         \RETURNS BOOLEAN AS $$\n\
         \  SELECT COUNT(*) = 1 FROM\n\
         \    (SELECT * FROM users WHERE name = $1 AND \
         \                               password = crypt($2, pwsalt)) t\n\
         \$$ LANGUAGE SQL;\n\
         \\n\
         \CREATE TABLE weighins (\n\
         \  name VARCHAR REFERENCES users,\n\
         \  measured_on DATE,\n\
         \  weight INTEGER NOT NULL,\n\
         \  PRIMARY KEY (name, measured_on)\n\
         \);\n\
         \\n\
         \CREATE TABLE schema_version (\n\
         \  lock CHAR(1) PRIMARY KEY DEFAULT('X'),\n\
         \  version INTEGER NOT NULL\n\
         \  CHECK(lock = 'X')\n\
         \);\n\
         \\n\
         \INSERT INTO schema_version (version) VALUES (1);\n\
         \\n\
         \CREATE VIEW last_weeks_loss AS\n\
         \WITH \n\
         \  -- First we need all the last weeks\n\
         \  last AS \n\
         \    (SELECT name, MAX(measured_on) AS measured_on \n\
         \     FROM weighins \n\
         \     GROUP BY name), \n\
         \     \n\
         \  -- Next we need the previous weigh-ins\n\
         \  previous AS \n\
         \    (SELECT weighins.name, MAX(weighins.measured_on) AS measured_on\n\
         \     FROM weighins\n\
         \     JOIN last ON last.name = weighins.name\n\
         \     WHERE weighins.measured_on < last.measured_on\n\
         \     GROUP BY weighins.name),\n\
         \\n\
         \  -- Now we can get the last and previous weights\n\
         \  last_weights AS \n\
         \    (SELECT name, weight FROM weighins NATURAL JOIN last),\n\
         \\n\  
         \  previous_weights AS \n\
         \    (SELECT name, weight FROM weighins NATURAL JOIN previous),\n\
         \  \n\
         \  -- now we can combine them and calculate their difference\n\
         \  lossage AS \n\
         \    (SELECT lw.name, pw.weight - lw.weight as loss \n\
         \     FROM last_weights AS lw\n\
         \     JOIN previous_weights AS pw ON (lw.name = pw.name)\n\
         \     WHERE lw.weight < pw.weight),\n\
         \\n\
         \  total_loss AS \n\
         \    (SELECT SUM(loss) AS total_loss FROM lossage)\n\
         \\n\
         \SELECT name, loss::numeric / total_loss.total_loss * 100 AS pct_lost\n\
         \FROM lossage, total_loss;"

-- Version 2 will fill in this function to migrate the database for its needs
upgradeSchema 1 dbh = return ()

-- We must be farther along than this codebase knows how to deal with
upgradeSchema _ dbh = fail $ "Schema is too new for this version of the code"