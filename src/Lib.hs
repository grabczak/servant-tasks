module Lib (createHash, verifyHash) where

import Data.Password.Argon2
import qualified Data.Text as T

createHash :: String -> IO T.Text
createHash password = do
    let plain = mkPassword $ T.pack password
    hashed <- hashPassword plain
    return $ T.pack $ show hashed

verifyHash :: String -> T.Text -> IO Bool
verifyHash password hash = do
    let plain = mkPassword $ T.pack password
    let hashed = read $ T.unpack hash
    return $ case checkPassword plain hashed of
        PasswordCheckSuccess -> True
        _ -> False
