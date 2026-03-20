{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handlers (register, login, userGet) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSC
import Servant
import Servant.Auth.Server

import API
import DB

register :: UserAuth -> Handler UserData
register UserAuth{name, password} = do
  user <- liftIO $ selectUserByName name
  case user of
    Just _ -> throwError err409{errBody = "User already exists"}
    Nothing -> do
      id <- liftIO $ insertUser UserAuth{name, password}
      user <- liftIO $ selectUserById id
      case user of
        Nothing -> throwError err500{errBody = "Failed to retrieve user after registration"}
        Just user -> return user

login ::
  CookieSettings ->
  JWTSettings ->
  UserAuth ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
login cookieSettings jwtSettings UserAuth{name = _name, password = _password} = do
  user <- liftIO $ selectUserByCredentials _name _password
  case user of
    Nothing -> throwError err401{errBody = "Invalid credentials"}
    Just user -> do
      loginAccepted <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case loginAccepted of
        Nothing -> throwError err401{errBody = "Login failed"}
        Just x -> do
          jwt <- liftIO $ makeJWT user jwtSettings Nothing
          case jwt of
            Left _ -> throwError err401{errBody = "JWT creation failed"}
            Right r -> return $ x (BSC.unpack r)

userGet :: AuthResult UserData -> Handler UserData
userGet (Authenticated UserData{id}) = do
  user <- liftIO $ selectUserById id
  case user of
    Nothing -> throwError err404{errBody = "User not found"}
    Just user -> return user
userGet _ = throwError err401{errBody = "Authentication required"}
