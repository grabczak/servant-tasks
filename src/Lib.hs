{-# LANGUAGE OverloadedStrings #-}

module Lib (createHash, verifyHash) where

import Data.Password.Argon2
import qualified Data.Text as T

createHash :: String -> IO String
createHash str = do
  let plain = mkPassword $ T.pack str
  hashed <- hashPassword plain
  return $ show hashed

verifyHash :: String -> String -> IO Bool
verifyHash str hash = do
  let plain = mkPassword $ T.pack str
  let hashed = read hash
  return $ case checkPassword plain hashed of
    PasswordCheckSuccess -> True
    _ -> False
