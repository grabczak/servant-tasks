{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handlers (
  register,
  login,
  userGet,
  userPut,
  tasksGet,
  tasksPost,
  taskGet,
  taskPut,
  taskDelete,
) where

import Control.Monad.IO.Class
import Servant
import Servant.Auth.Server

import API
import DB
import Lib

register :: UserAuth -> Handler UserData
register UserAuth{name, password} = do
  user <- liftIO $ selectUserDataByName name
  case user of
    Just _ -> throwError err409{errBody = "User already exists"}
    Nothing -> do
      hashed <- liftIO $ createHash password
      id <- liftIO $ insertUser UserAuth{name, password = hashed}
      user <- liftIO $ selectUserDataById id
      case user of
        Nothing -> throwError err500{errBody = "Failed to retrieve user after registration"}
        Just user -> return user

login :: CookieSettings -> JWTSettings -> UserAuth -> Handler (AuthHeaders String)
login cookieSettings jwtSettings UserAuth{name, password} = do
  user <- liftIO $ selectUserFullByName name
  case user of
    Nothing -> throwError err401{errBody = "Invalid credentials"}
    Just UserFull{id, password = hash} -> do
      isValid <- liftIO $ verifyHash password hash
      case isValid of
        False -> throwError err401{errBody = "Invalid credentials"}
        True -> do
          sessionResult <- liftIO $ createSession cookieSettings jwtSettings UserToken{id}
          case sessionResult of
            Left err -> throwError err
            Right authHeaders -> return authHeaders

userGet :: AuthResult UserToken -> Handler UserData
userGet (Authenticated UserToken{id}) = do
  user <- liftIO $ selectUserDataById id
  case user of
    Nothing -> throwError err404{errBody = "User not found"}
    Just user -> return user
userGet _ = throwError err401{errBody = "Authentication required"}

userPut :: AuthResult UserToken -> UserPut -> Handler UserData
userPut (Authenticated UserToken{id}) UserPut{name, oldPassword, newPassword} = do
  user <- liftIO $ selectUserFullById id
  case user of
    Nothing -> throwError err404{errBody = "User not found"}
    Just UserFull{password = hash} -> do
      isValid <- liftIO $ verifyHash oldPassword hash
      case isValid of
        False -> throwError err401{errBody = "Invalid credentials"}
        True -> do
          hashed <- liftIO $ createHash newPassword
          updatedUser <- liftIO $ updateUserById id UserAuth{name, password = hashed}
          case updatedUser of
            Nothing -> throwError err500{errBody = "Failed to update user"}
            Just user -> return user
userPut _ _ = throwError err401{errBody = "Authentication required"}

tasksGet :: AuthResult UserToken -> Handler [TaskFull]
tasksGet (Authenticated UserToken{id}) = do
  tasks <- liftIO $ selectTasksByUserId id
  return tasks
tasksGet _ = throwError err401{errBody = "Authentication required"}

tasksPost :: AuthResult UserToken -> TaskCreate -> Handler TaskFull
tasksPost (Authenticated UserToken{id}) taskCreate = do
  task <- liftIO $ insertTaskByUserId id taskCreate
  case task of
    Nothing -> throwError err500{errBody = "Failed to create task"}
    Just task -> return task
tasksPost _ _ = throwError err401{errBody = "Authentication required"}

taskGet :: AuthResult UserToken -> Int -> Handler TaskFull
taskGet (Authenticated UserToken{id}) taskId = do
  task <- liftIO $ selectTaskById taskId
  case task of
    Nothing -> throwError err404{errBody = "Task not found"}
    Just task ->
      if userId task /= id
        then throwError err403{errBody = "Forbidden"}
        else return task
taskGet _ _ = throwError err401{errBody = "Authentication required"}

taskPut :: AuthResult UserToken -> Int -> TaskCreate -> Handler TaskFull
taskPut (Authenticated UserToken{id}) taskId taskCreate = do
  task <- liftIO $ selectTaskById taskId
  case task of
    Nothing -> throwError err404{errBody = "Task not found"}
    Just task ->
      if userId task /= id
        then throwError err403{errBody = "Forbidden"}
        else do
          updatedTask <- liftIO $ updateTaskById taskId taskCreate
          case updatedTask of
            Nothing -> throwError err500{errBody = "Failed to update task"}
            Just task -> return task
taskPut _ _ _ = throwError err401{errBody = "Authentication required"}

taskDelete :: AuthResult UserToken -> Int -> Handler NoContent
taskDelete (Authenticated UserToken{id}) taskId = do
  task <- liftIO $ selectTaskById taskId
  case task of
    Nothing -> throwError err404{errBody = "Task not found"}
    Just task ->
      if userId task /= id
        then throwError err403{errBody = "Forbidden"}
        else do
          liftIO $ deleteTaskById taskId
          return NoContent
taskDelete _ _ = throwError err401{errBody = "Authentication required"}
