{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module API (UserAuth (..), UserData (..), UserFull (..), AuthHeaders, API) where

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
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow)

-- API response
data UserData = UserData
  { id :: Int
  , name :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow, FromJWT, ToJWT)

-- Internal record
data UserFull = UserFull
  { id :: Int
  , name :: String
  , password :: String
  }
  deriving (Eq, Show, Generic, FromRow)

type AuthHeaders = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]

type Protected = Auth '[Cookie, JWT] UserData

type API =
  "auth" :> "register" :> ReqBody '[JSON] UserAuth :> PostCreated '[JSON] UserData
    :<|> "auth" :> "login" :> ReqBody '[JSON] UserAuth :> Post '[JSON] (AuthHeaders String)
    :<|> Protected :> "user" :> "me" :> Get '[JSON] UserData
