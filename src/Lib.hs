{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib (createHash, verifyHash, createSession) where

import Data.Password.Argon2
import qualified Data.Text as T

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Time.Clock
import Servant
import Servant.Auth.Server

import API

createHash :: String -> IO String
createHash password = do
  let plain = mkPassword $ T.pack password
  hashed <- hashPassword plain
  return $ show hashed

verifyHash :: String -> String -> IO Bool
verifyHash password hash = do
  let plain = mkPassword $ T.pack password
  let hashed = read hash
  return $ case checkPassword plain hashed of
    PasswordCheckSuccess -> True
    _ -> False

createSession :: CookieSettings -> JWTSettings -> UserToken -> IO (Either ServerError (AuthHeaders String))
createSession cookieSettings jwtSettings user = do
  loginAccepted <- acceptLogin cookieSettings jwtSettings user
  case loginAccepted of
    Nothing -> return $ Left err401{errBody = "Login failed"}
    Just headerBuilder -> do
      now <- liftIO getCurrentTime
      let expiry = addUTCTime (secondsToNominalDiffTime 2592000) now -- One month expiry
      jwt <- liftIO $ makeJWT user jwtSettings (Just expiry)
      case jwt of
        Left _ -> return $ Left err401{errBody = "JWT creation failed"}
        Right token -> return $ Right $ headerBuilder (BSC.unpack token)
