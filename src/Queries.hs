{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries (
  createDB,
  insertUser,
  selectUserDataById,
  selectUserDataByName,
  selectUserFullById,
  selectUserFullByName,
  updateUserById,
  selectTaskById,
  selectTasksByUserId,
  insertTaskByUserId,
  updateTaskById,
  deleteTaskById,
) where

import Database.SQLite.Simple

import Api

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

insertUser :: UserAuth -> IO (Maybe UserData)
insertUser user = withConnection dbName $ \conn -> do
  execute conn "INSERT INTO users (name, password) VALUES (?, ?)" (user.name, user.password)
  createdUserId <- lastInsertRowId conn
  selectUserDataById $ fromIntegral createdUserId

selectUserDataById :: Int -> IO (Maybe UserData)
selectUserDataById userId = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name FROM users WHERE id = ?" (Only userId)
  return $ selectFirst result

selectUserDataByName :: String -> IO (Maybe UserData)
selectUserDataByName name = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name FROM users WHERE name = ?" (Only name)
  return $ selectFirst result

selectUserFullById :: Int -> IO (Maybe UserFull)
selectUserFullById userId = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name, password FROM users WHERE id = ?" (Only userId)
  return $ selectFirst result

selectUserFullByName :: String -> IO (Maybe UserFull)
selectUserFullByName name = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, name, password FROM users WHERE name = ?" (Only name)
  return $ selectFirst result

updateUserById :: Int -> UserAuth -> IO (Maybe UserData)
updateUserById userId user = withConnection dbName $ \conn -> do
  execute conn "UPDATE users SET name = ?, password = ? WHERE id = ?" (user.name, user.password, userId)
  selectUserDataById userId

selectTaskById :: Int -> IO (Maybe TaskFull)
selectTaskById taskId = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, user_id, title, description, completed FROM tasks WHERE id = ?" (Only taskId)
  return $ selectFirst result

selectTasksByUserId :: Int -> IO [TaskFull]
selectTasksByUserId userId = withConnection dbName $ \conn -> do
  result <- query conn "SELECT id, user_id, title, description, completed FROM tasks WHERE user_id = ?" (Only userId)
  return result

insertTaskByUserId :: Int -> TaskCreate -> IO (Maybe TaskFull)
insertTaskByUserId userId task = withConnection dbName $ \conn -> do
  execute conn "INSERT INTO tasks (user_id, title, description, completed) VALUES (?, ?, ?, ?)" (userId, task.title, task.description, task.completed)
  taskId <- lastInsertRowId conn
  selectTaskById $ fromIntegral taskId

updateTaskById :: Int -> TaskCreate -> IO (Maybe TaskFull)
updateTaskById taskId task = withConnection dbName $ \conn -> do
  execute conn "UPDATE tasks SET title = ?, description = ?, completed = ? WHERE id = ?" (task.title, task.description, task.completed, taskId)
  selectTaskById taskId

deleteTaskById :: Int -> IO ()
deleteTaskById taskId = withConnection dbName $ \conn -> do
  execute conn "DELETE FROM tasks WHERE id = ?" (Only taskId)
