{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DB (
  createDB,
  insertUser,
  selectUserByName,
  selectUserById,
  selectUserByCredentials,
) where

import Database.SQLite.Simple

import API
import Lib

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

insertUser :: UserAuth -> IO Int
insertUser UserAuth{name, password} = withConnection dbName $ \conn -> do
  hashed <- createHash password
  execute conn "INSERT INTO users (name, password) VALUES (?, ?)" (name, hashed)
  userId <- lastInsertRowId conn
  return $ fromIntegral userId

selectUserByName :: String -> IO (Maybe UserData)
selectUserByName name = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name FROM users WHERE name = ?" (Only name)
  return $ case result of
    [] -> Nothing
    (user : _) -> Just user

selectUserById :: Int -> IO (Maybe UserData)
selectUserById id = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name FROM users WHERE id = ?" (Only id)
  return $ case result of
    [] -> Nothing
    (user : _) -> Just user

selectUserByCredentials :: String -> String -> IO (Maybe UserFull)
selectUserByCredentials name password = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name, password FROM users WHERE name = ? AND password =  ?" (name, password)
  return $ case result of
    [] -> Nothing
    (user : _) -> Just user
