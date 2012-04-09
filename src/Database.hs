module Database where

import Control.Applicative

import Database.HDBC
import Database.HDBC.PostgreSQL

setupDatabase :: (IConnection c) => c -> IO ()
setupDatabase dbh = do
  version <- schemaVersion dbh
  upgradeSchema version dbh
  commit dbh

schemaVersion :: (IConnection c) => c -> IO Integer
schemaVersion dbh = do
  tables <- getTables dbh
  version <- if "schema_version" `elem` tables
    then (fromSql . head . head) <$> quickQuery' dbh versionQuery []
    else return 0
  return version
    where
      versionQuery = "SELECT version FROM schema_version"

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
         \INSERT INTO schema_version (version) VALUES (1);"

upgradeSchema 1 dbh = return ()

upgradeSchema _ dbh = fail $ "Schema is too new for this version of the code"