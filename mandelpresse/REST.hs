{-# LANGUAGE GADTs, OverloadedStrings, TemplateHaskell #-}
{-
module REST ( getUserById
            , getUserByName
            , getProductById
            , getProductByEAN
            , getProductByName
            , getTransactionById
            , getAllUsers
            , getAllProducts
            , getAllTransactions
            ) where
-}
module REST where

import Types

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower)
import Data.Text (Text)
import Network.HTTP.Conduit

data JSONList a = FromJSON a => JSONList { list_items :: [a]
                                         , list_hasmore :: Bool
                                         }

$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''TransactionType)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Product)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Transaction)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''User)
$(deriveFromJSON defaultOptions {fieldLabelModifier = drop 5} ''JSONList)

baseURL = "https://gaf.fs.lmu.de/saftladen/skruppy/index.php"

fetch :: String -> IO B.ByteString
fetch s = do
    withManager $ \manager -> do
        req <- liftIO $ parseUrl (baseURL ++ s)
        let req' = req { requestHeaders = [("Content-Type", "application/json")]}
        responseBody <$> httpLbs req' manager

add :: String -> B.ByteString -> IO Bool
add s input = do
    withManager $ \manager -> do
        req <- liftIO $ parseUrl (baseURL ++ s)
        let req' = req { requestHeaders = [("Content-Type", "application/json")]
                       , method = "POST"
                       , requestBody = (RequestBodyLBS input)}
        ret <- responseBody <$> httpLbs req' manager
        if ret == "null"
           then return True
           else return False

delete :: String -> IO B.ByteString
delete s = do
    withManager $ \manager -> do
        req <- liftIO $ parseUrl (baseURL ++ s)
        let req' = req { requestHeaders = [("Content-Type", "application/json")]
                       , method = "DELETE"}
        responseBody <$> httpLbs req' manager

buildArgs :: Show a => [(String,a)] -> String
buildArgs [] = ""
buildArgs args = '?' : (tail $ foldl (\a -> \(s,v) -> a ++ "&" ++ s ++ "=" ++ show v) "" args)

getUserById :: Int -> IO (Either String User)
getUserById i = eitherDecode <$> fetch ("/users/id/" ++ show i)

getUserByName :: String -> IO (Either String User)
getUserByName n = eitherDecode <$> fetch ("/users/name/" ++ n)

getProductById :: Int -> IO (Either String Product)
getProductById i = eitherDecode <$> fetch ("/products/id/" ++ show i)

getProductByEAN :: Integer -> IO (Either String Product)
getProductByEAN n = eitherDecode <$> fetch ("/products/ean/" ++ show n)

getProductByName :: String -> IO (Either String Product)
getProductByName n = eitherDecode <$> fetch ("/products/name/" ++ n)

getTransactionById :: Int -> IO (Either String Transaction)
getTransactionById i = eitherDecode <$> fetch ("/transactions/" ++ show i)

getAllProducts :: IO [Product]
getAllProducts = getAll "/products/"

getAllUsers :: IO [User]
getAllUsers = getAll "/users/"

getAllTransactions :: IO [Transaction]
getAllTransactions = getAll "/transactions/"

addUser :: User -> IO Bool
addUser u = add "/users/" (encode u)

getAll path = getAll' 100 0
  where
    getAll' range offset = do
      list <- getRange path range offset
      case list of
        Left e -> return []
        Right l -> if list_hasmore l
                      then (list_items l ++) <$> getAll' range (offset + range)
                      else return $ list_items l

getRange path range offset =
  eitherDecode <$> fetch (path ++ buildArgs [("max", range), ("offset", offset)])
