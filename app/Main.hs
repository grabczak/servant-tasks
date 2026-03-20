{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server

import API
import DB
import Handlers

port :: Int
port = 8080

proxy :: Proxy API
proxy = Proxy

server :: CookieSettings -> JWTSettings -> Server API
server cookieSettings jwtSettings =
  register
    :<|> login cookieSettings jwtSettings
    :<|> userGet

main :: IO ()
main = do
  createDB

  jwtSecretKey <- generateKey

  let cookieSettings = defaultCookieSettings
  let jwtSettings = defaultJWTSettings jwtSecretKey
  let config = cookieSettings :. jwtSettings :. EmptyContext

  print $ "Running on port " ++ show port
  run port $ serveWithContext proxy config $ server cookieSettings jwtSettings
