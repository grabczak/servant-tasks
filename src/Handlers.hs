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

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Maybe
import Data.Time
import Servant
import Servant.Auth.Server

import Api
import Lib
import Queries

register :: UserAuth -> Handler UserData
register userAuth = do
  user <- liftIO $ selectUserDataByName userAuth.name
  when (isJust user) $ throwError err409{errBody = "User already exists"}

  hashedPassword <- liftIO $ createHash userAuth.password
  createdUserId <- liftIO $ insertUser UserAuth{name = userAuth.name, password = hashedPassword}
  createdUser <-
    liftIO (selectUserDataById createdUserId)
      >>= maybe (throwError err500{errBody = "Failed to retrieve user after registration"}) return

  return createdUser

login :: CookieSettings -> JWTSettings -> UserAuth -> Handler (AuthHeaders String)
login cookieSettings jwtSettings userAuth = do
  user <-
    liftIO (selectUserFullByName userAuth.name)
      >>= maybe (throwError err401{errBody = "Invalid credentials"}) return

  passwordValid <- liftIO $ verifyHash userAuth.password user.password
  unless passwordValid $ throwError err401{errBody = "Invalid credentials"}

  let token = UserToken{id = user.id}

  headerBuilder <-
    liftIO (acceptLogin cookieSettings jwtSettings token)
      >>= maybe (throwError err401{errBody = "Login failed"}) return

  now <- liftIO getCurrentTime
  let oneMonth = secondsToNominalDiffTime 2592000
  let expiry = addUTCTime oneMonth now

  jwt <-
    liftIO (makeJWT token jwtSettings (Just expiry))
      >>= either (const $ throwError err401{errBody = "JWT creation failed"}) return

  return $ headerBuilder (BSC.unpack jwt)

userGet :: AuthResult UserToken -> Handler UserData
userGet (Authenticated userToken) = do
  user <-
    liftIO (selectUserDataById userToken.id)
      >>= maybe (throwError err404{errBody = "User not found"}) return

  return user
userGet _ = throwError err401{errBody = "Authentication required"}

userPut :: AuthResult UserToken -> UserUpdate -> Handler UserData
userPut (Authenticated userToken) userUpdate = do
  user <-
    liftIO (selectUserFullById userToken.id)
      >>= maybe (throwError err404{errBody = "User not found"}) return

  passwordValid <- liftIO (verifyHash userUpdate.oldPassword user.password)
  unless passwordValid $ throwError err401{errBody = "Invalid credentials"}

  hashedPassword <- liftIO $ createHash userUpdate.newPassword

  updatedUser <-
    liftIO (updateUserById user.id UserAuth{name = userUpdate.name, password = hashedPassword})
      >>= maybe (throwError err500{errBody = "Failed to update user"}) return

  return updatedUser
userPut _ _ = throwError err401{errBody = "Authentication required"}

tasksGet :: AuthResult UserToken -> Handler [TaskFull]
tasksGet (Authenticated userToken) = do
  tasks <- liftIO $ selectTasksByUserId userToken.id
  return tasks
tasksGet _ = throwError err401{errBody = "Authentication required"}

tasksPost :: AuthResult UserToken -> TaskCreate -> Handler TaskFull
tasksPost (Authenticated userToken) taskCreate = do
  task <-
    liftIO (insertTaskByUserId userToken.id taskCreate)
      >>= maybe (throwError err500{errBody = "Failed to create task"}) return

  return task
tasksPost _ _ = throwError err401{errBody = "Authentication required"}

taskGet :: AuthResult UserToken -> Int -> Handler TaskFull
taskGet (Authenticated userToken) taskId = do
  task <-
    liftIO (selectTaskById taskId)
      >>= maybe (throwError err404{errBody = "Task not found"}) return

  when (task.userId /= userToken.id) $ throwError err403{errBody = "Forbidden"}

  return task
taskGet _ _ = throwError err401{errBody = "Authentication required"}

taskPut :: AuthResult UserToken -> Int -> TaskCreate -> Handler TaskFull
taskPut (Authenticated userToken) taskId taskCreate = do
  task <-
    liftIO (selectTaskById taskId)
      >>= maybe (throwError err404{errBody = "Task not found"}) return

  when (task.userId /= userToken.id) $ throwError err403{errBody = "Forbidden"}

  updatedTask <-
    liftIO (updateTaskById taskId taskCreate)
      >>= maybe (throwError err500{errBody = "Failed to update task"}) return

  return updatedTask
taskPut _ _ _ = throwError err401{errBody = "Authentication required"}

taskDelete :: AuthResult UserToken -> Int -> Handler NoContent
taskDelete (Authenticated userToken) taskId = do
  task <-
    liftIO (selectTaskById taskId)
      >>= maybe (throwError err404{errBody = "Task not found"}) return

  when (task.userId /= userToken.id) $
    throwError err403{errBody = "Forbidden"}

  liftIO $ deleteTaskById taskId

  return NoContent
taskDelete _ _ = throwError err401{errBody = "Authentication required"}
