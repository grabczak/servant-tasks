{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module API (
  UserAuth (..),
  UserData (..),
  UserFull (..),
  UserPut (..),
  UserToken (..),
  TaskCreate (..),
  TaskFull (..),
  AuthHeaders,
  API,
) where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics
import Servant
import Servant.Auth.Server

-- Credentials
data UserAuth = UserAuth
  { name :: String
  , password :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- API response
data UserData = UserData
  { id :: Int
  , name :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow)

-- Internal record
data UserFull = UserFull
  { id :: Int
  , name :: String
  , password :: String
  }
  deriving (Eq, Show, Generic, FromRow)

-- User put request
data UserPut = UserPut
  { name :: String
  , oldPassword :: String
  , newPassword :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Token content
data UserToken = UserToken
  { id :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

-- Task create request
data TaskCreate = TaskCreate
  { title :: String
  , description :: String
  , completed :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Task record
data TaskFull = TaskFull
  { id :: Int
  , userId :: Int
  , title :: String
  , description :: String
  , completed :: Bool
  }
  deriving (Eq, Show, Generic, FromRow, FromJSON, ToJSON)

type AuthHeaders = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]

type Protected = Auth '[Cookie, JWT] UserToken

type API =
  "auth" :> "register" :> ReqBody '[JSON] UserAuth :> PostCreated '[JSON] UserData
    :<|> "auth" :> "login" :> ReqBody '[JSON] UserAuth :> Post '[JSON] (AuthHeaders String)
    :<|> Protected :> "user" :> "me" :> Get '[JSON] UserData
    :<|> Protected :> "user" :> "me" :> ReqBody '[JSON] UserPut :> Put '[JSON] UserData
    :<|> Protected :> "user" :> "tasks" :> Get '[JSON] [TaskFull]
    :<|> Protected :> "user" :> "task" :> ReqBody '[JSON] TaskCreate :> PostCreated '[JSON] TaskFull
    :<|> Protected :> "user" :> "task" :> Capture "taskId" Int :> Get '[JSON] TaskFull
    :<|> Protected :> "user" :> "task" :> Capture "taskId" Int :> ReqBody '[JSON] TaskCreate :> Put '[JSON] TaskFull
    :<|> Protected :> "user" :> "task" :> Capture "taskId" Int :> Delete '[JSON] NoContent
