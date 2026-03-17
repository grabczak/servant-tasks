{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module API (User (..), Login (..), API) where

import Data.Aeson (FromJSON, ToJSON)
import Database.SQLite.Simple (FromRow)
import GHC.Generics (Generic)
import Servant (JSON, PostCreated, ReqBody, type (:>))

data User = User
  { id :: Int
  , name :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow)

data Login = Login
  { name :: String
  , password :: String
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type API = "auth" :> "register" :> ReqBody '[JSON] Login :> PostCreated '[JSON] User
