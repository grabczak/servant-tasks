module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant (Proxy (..), Server, serve)

import API (API)
import Handlers (register)

port :: Int
port = 8080

api :: Proxy API
api = Proxy

server :: Server API
server = register

main :: IO ()
main = do
  print $ "Running on port " ++ show port
  run port $ serve api server
