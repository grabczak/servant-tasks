{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Api
import Lib
import Queries

register :: UserAuth -> Handler UserData
register userAuth = do
  mUser <- liftIO $ selectUserDataByName userAuth.name
  case mUser of
    Just _ -> throwError err409{errBody = "User already exists"}
    Nothing -> do
      hashedPassword <- liftIO $ createHash userAuth.password
      createdUserId <- liftIO $ insertUser UserAuth{name = userAuth.name, password = hashedPassword}
      mCreatedUser <- liftIO $ selectUserDataById createdUserId
      case mCreatedUser of
        Nothing -> throwError err500{errBody = "Failed to retrieve user after registration"}
        Just createdUser -> return createdUser

login :: CookieSettings -> JWTSettings -> UserAuth -> Handler (AuthHeaders String)
login cookieSettings jwtSettings userAuth = do
  mUser <- liftIO $ selectUserFullByName userAuth.name
  case mUser of
    Nothing -> throwError err401{errBody = "Invalid credentials"}
    Just user -> do
      isValid <- liftIO $ verifyHash userAuth.password user.password
      case isValid of
        False -> throwError err401{errBody = "Invalid credentials"}
        True -> do
          mSession <- liftIO $ createSession cookieSettings jwtSettings UserToken{id = user.id}
          case mSession of
            Left err -> throwError err
            Right session -> return session

userGet :: AuthResult UserToken -> Handler UserData
userGet (Authenticated userToken) = do
  mUser <- liftIO $ selectUserDataById userToken.id
  case mUser of
    Nothing -> throwError err404{errBody = "User not found"}
    Just user -> return user
userGet _ = throwError err401{errBody = "Authentication required"}

userPut :: AuthResult UserToken -> UserUpdate -> Handler UserData
userPut (Authenticated userToken) userUpdate = do
  mUser <- liftIO $ selectUserFullById userToken.id
  case mUser of
    Nothing -> throwError err404{errBody = "User not found"}
    Just user -> do
      isValid <- liftIO $ verifyHash userUpdate.oldPassword user.password
      case isValid of
        False -> throwError err401{errBody = "Invalid credentials"}
        True -> do
          hashedPassword <- liftIO $ createHash userUpdate.newPassword
          mUpdatedUser <- liftIO $ updateUserById user.id UserAuth{name = userUpdate.name, password = hashedPassword}
          case mUpdatedUser of
            Nothing -> throwError err500{errBody = "Failed to update user"}
            Just updatedUser -> return updatedUser
userPut _ _ = throwError err401{errBody = "Authentication required"}

tasksGet :: AuthResult UserToken -> Handler [TaskFull]
tasksGet (Authenticated userToken) = do
  tasks <- liftIO $ selectTasksByUserId userToken.id
  return tasks
tasksGet _ = throwError err401{errBody = "Authentication required"}

tasksPost :: AuthResult UserToken -> TaskCreate -> Handler TaskFull
tasksPost (Authenticated userToken) taskCreate = do
  mTask <- liftIO $ insertTaskByUserId userToken.id taskCreate
  case mTask of
    Nothing -> throwError err500{errBody = "Failed to create task"}
    Just task -> return task
tasksPost _ _ = throwError err401{errBody = "Authentication required"}

taskGet :: AuthResult UserToken -> Int -> Handler TaskFull
taskGet (Authenticated userToken) taskId = do
  mTask <- liftIO $ selectTaskById taskId
  case mTask of
    Nothing -> throwError err404{errBody = "Task not found"}
    Just task ->
      if userId task /= userToken.id
        then throwError err403{errBody = "Forbidden"}
        else return task
taskGet _ _ = throwError err401{errBody = "Authentication required"}

taskPut :: AuthResult UserToken -> Int -> TaskCreate -> Handler TaskFull
taskPut (Authenticated userToken) taskId taskCreate = do
  mTask <- liftIO $ selectTaskById taskId
  case mTask of
    Nothing -> throwError err404{errBody = "Task not found"}
    Just task ->
      if userId task /= userToken.id
        then throwError err403{errBody = "Forbidden"}
        else do
          mUpdatedTask <- liftIO $ updateTaskById taskId taskCreate
          case mUpdatedTask of
            Nothing -> throwError err500{errBody = "Failed to update task"}
            Just updatedTask -> return updatedTask
taskPut _ _ _ = throwError err401{errBody = "Authentication required"}

taskDelete :: AuthResult UserToken -> Int -> Handler NoContent
taskDelete (Authenticated userToken) taskId = do
  mTask <- liftIO $ selectTaskById taskId
  case mTask of
    Nothing -> throwError err404{errBody = "Task not found"}
    Just task ->
      if userId task /= userToken.id
        then throwError err403{errBody = "Forbidden"}
        else do
          liftIO $ deleteTaskById taskId
          return NoContent
taskDelete _ _ = throwError err401{errBody = "Authentication required"}
