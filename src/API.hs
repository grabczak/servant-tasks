{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module API (UserAuth (..), UserData (..), UserFull (..), API) where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics
import Servant
import Servant.Auth.Server

-- Credentials: name and password
data UserAuth = UserAuth
  { name :: String
  , password :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow)

-- Public API response: id and name only
data UserData = UserData
  { id :: Int
  , name :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow, FromJWT, ToJWT)

-- Full internal record: id, name, and password
data UserFull = UserFull
  { id :: Int
  , name :: String
  , password :: String
  }
  deriving (Eq, Show, Generic, FromRow)

type API =
  "auth" :> "register" :> ReqBody '[JSON] UserAuth :> PostCreated '[JSON] UserData
    :<|> "auth" :> "login" :> ReqBody '[JSON] UserAuth :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
