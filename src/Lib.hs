module Lib (createHash, verifyHash) where

import Data.Password.Argon2
import qualified Data.Text as T

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
