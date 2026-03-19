{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handlers (register, login) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text as T
import Servant
import Servant.Auth.Server

import API
import DB
import Lib

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
login cookieSettings jwtSettings UserAuth{name, password} = do
  user <- liftIO $ selectUserByCredentials name password
  case user of
    Nothing -> throwError err401{errBody = "Invalid credentials"}
    Just UserFull{id, name, password = hashedPassword} -> do
      isValid <- liftIO $ verifyHash password (T.pack hashedPassword)
      case isValid of
        False -> throwError err401{errBody = "Invalid credentials"}
        True -> do
          loginAccepted <- liftIO $ acceptLogin cookieSettings jwtSettings UserData{id, name}
          case loginAccepted of
            Nothing -> throwError err401{errBody = "Login failed"}
            Just x -> do
              jwt <- liftIO $ makeJWT UserData{id, name} jwtSettings Nothing
              case jwt of
                Left _ -> throwError err401{errBody = "JWT creation failed"}
                Right r -> return $ x (BSC.unpack r)
