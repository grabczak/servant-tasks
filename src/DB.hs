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
  updateUserById,
) where

import qualified Data.Text as T
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

selectFirst :: [a] -> Maybe a
selectFirst [] = Nothing
selectFirst (x : _) = Just x

insertUser :: UserAuth -> IO Int
insertUser UserAuth{name, password} = withConnection dbName $ \conn -> do
  hashed <- createHash password
  execute conn "INSERT INTO users (name, password) VALUES (?, ?)" (name, hashed)
  userId <- lastInsertRowId conn
  return $ fromIntegral userId

selectUserByName :: String -> IO (Maybe UserData)
selectUserByName name = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name FROM users WHERE name = ?" (Only name)
  return $ selectFirst result

selectUserById :: Int -> IO (Maybe UserData)
selectUserById id = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name FROM users WHERE id = ?" (Only id)
  return $ selectFirst result

selectUserByCredentials :: String -> String -> IO (Maybe UserData)
selectUserByCredentials _name _password = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name, password FROM users WHERE name = ?" (Only _name)
  case selectFirst result of
    Nothing -> return Nothing
    Just UserFull{id, name, password} -> do
      isValid <- verifyHash _password (T.pack password)
      return $ case isValid of
        False -> Nothing
        True -> Just $ UserData{id, name}

updateUserById :: Int -> UserAuth -> IO (Maybe UserData)
updateUserById id UserAuth{name, password} = withConnection dbName $ \conn -> do
  hashed <- createHash password
  execute conn "UPDATE users SET name = ?, password = ? WHERE id = ?" (name, hashed, id)
  selectUserById id
