{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DB (createDB, insertUser, selectUserByName, selectUserById) where

import Database.SQLite.Simple (
  Only (Only),
  Query (Query),
  execute,
  execute_,
  lastInsertRowId,
  query,
  withConnection,
 )

import API (Login (..), User)

dbName :: String
dbName = "app.db"

createDB :: IO ()
createDB = withConnection dbName $ \conn -> do
  execute_ conn $
    Query $
      "CREATE TABLE IF NOT EXISTS users ("
        <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
        <> "name TEXT UNIQUE NOT NULL,"
        <> "password TEXT NOT NULL"
        <> ")"

insertUser :: Login -> IO Int
insertUser Login{name, password} = withConnection dbName $ \conn -> do
  execute conn "INSERT INTO users (name, password) VALUES (?, ?)" (name, password)
  userId <- lastInsertRowId conn
  return $ fromIntegral userId

selectUserByName :: String -> IO (Maybe User)
selectUserByName name = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name FROM users WHERE name = ?" (Only name)
  return $ case result of
    [] -> Nothing
    (user : _) -> Just user

selectUserById :: Int -> IO (Maybe User)
selectUserById id = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name FROM users WHERE id = ?" (Only id)
  return $ case result of
    [] -> Nothing
    (user : _) -> Just user
