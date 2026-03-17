{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handlers (register) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Servant (
  Handler,
  ServerError (errBody),
  err409,
  err500,
  throwError,
 )

import API (Login (..), User)
import DB (insertUser, selectUserById, selectUserByName)

register :: Login -> Handler User
register Login{name, password} = do
  user <- liftIO $ selectUserByName name
  case user of
    Just _ -> throwError err409{errBody = "User already exists"}
    Nothing -> do
      id <- liftIO $ insertUser Login{name, password}
      user <- liftIO $ selectUserById id
      case user of
        Nothing -> throwError err500{errBody = "Failed to retrieve user after registration"}
        Just user -> return user
