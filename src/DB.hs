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
  selectTaskById,
  selectTasksByUserId,
  insertTaskByUserId,
  updateTaskById,
  deleteTaskById,
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
  execute_ conn $
    Query $
      "CREATE TABLE IF NOT EXISTS tasks ("
        <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
        <> "user_id INTEGER NOT NULL,"
        <> "title TEXT NOT NULL,"
        <> "description TEXT NOT NULL,"
        <> "completed BOOLEAN NOT NULL CHECK (completed IN (0, 1)),"
        <> "FOREIGN KEY(user_id) REFERENCES users(id)"
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
      isValid <- verifyHash _password password
      return $ case isValid of
        False -> Nothing
        True -> Just $ UserData{id, name}

updateUserById :: Int -> UserPut -> IO (Maybe UserData)
updateUserById id UserPut{name, oldPassword, newPassword} = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name, password FROM users WHERE name = ?" (Only name)
  case selectFirst result of
    Nothing -> return Nothing
    Just UserFull{password} -> do
      isValid <- verifyHash oldPassword password
      case isValid of
        False -> return Nothing
        True -> do
          hashed <- createHash newPassword
          execute conn "UPDATE users SET name = ?, password = ? WHERE id = ?" (name, hashed, id)
          selectUserById id

selectTaskById :: Int -> IO (Maybe TaskFull)
selectTaskById id = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, user_id, title, description, completed FROM tasks WHERE id = ?" (Only id)
  return $ selectFirst result

selectTasksByUserId :: Int -> IO [TaskFull]
selectTasksByUserId userId = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, user_id, title, description, completed FROM tasks WHERE user_id = ?" (Only userId)
  return result

insertTaskByUserId :: Int -> TaskCreate -> IO (Maybe TaskFull)
insertTaskByUserId userId TaskCreate{title, description, completed} = withConnection dbName $ \conn -> do
  execute conn "INSERT INTO tasks (user_id, title, description, completed) VALUES (?, ?, ?, ?)" (userId, title, description, completed)
  taskId <- lastInsertRowId conn
  selectTaskById $ fromIntegral taskId

updateTaskById :: Int -> TaskCreate -> IO (Maybe TaskFull)
updateTaskById taskId TaskCreate{title, description, completed} = withConnection dbName $ \conn -> do
  execute conn "UPDATE tasks SET title = ?, description = ?, completed = ? WHERE id = ?" (title, description, completed, taskId)
  selectTaskById taskId

deleteTaskById :: Int -> IO ()
deleteTaskById taskId = withConnection dbName $ \conn -> do
  execute conn "DELETE FROM tasks WHERE id = ?" (Only taskId)
