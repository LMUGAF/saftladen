{-# LANGUAGE GADTs, OverloadedStrings, TemplateHaskell #-}
{-
module REST ( get
            , update
            , add
            , delete
            , getUserById
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
import Control.Exception as X
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower)
import Data.Text (Text)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status

data JSONList a = FromJSON a => JSONList { list_items :: [a]
                                         , list_hasmore :: Bool
                                         }

$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''TransactionType)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Product)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Transaction)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''User)
$(deriveFromJSON defaultOptions {fieldLabelModifier = drop 5} ''JSONList)

baseURL = "https://gaf.fs.lmu.de/saftladen/skruppy/index.php"

errorFromResponse :: String -> Response LB.ByteString -> String
errorFromResponse path r = concat [
    "HTTP error while fetching ", baseURL, path, "\n",
    "Status code: ", show (statusCode status), " ", show (statusMessage status), "\n",
    "Message body: \n", show (responseBody r)
    ]
    where
       status = responseStatus r

fetch :: String -> (Request -> Request) -> IO (Response LB.ByteString)
fetch s reqMods = do
    withManager $ \manager -> do
        req <- liftIO $ parseUrl (baseURL ++ s)
        let req' = reqMods $ req { requestHeaders = [("Content-Type", "application/json")]
                                 , checkStatus = (\a b c -> Nothing) }
        httpLbs req' manager

get :: FromJSON a => String -> IO (Either String a)
get s = do
  r <- fetch s id
  case statusCode (responseStatus r) of
    200 -> return $ eitherDecode (responseBody r)
    otherwise -> return $ Left (errorFromResponse s r)

add :: String -> LB.ByteString -> IO (Either String ())
add s input = do
    r <- fetch s (\r -> r { method = "POST"
                          , requestBody = (RequestBodyLBS input)} )
    case statusCode (responseStatus r) of
      201 -> return $ Right ()
      otherwise -> return $ Left (errorFromResponse s r)

delete :: String -> IO (Either String ())
delete s = do
    r <- fetch s (\r -> r { method = "DELETE" })
    case statusCode (responseStatus r) of
      200 -> return $ Right ()
      otherwise -> return $ Left (errorFromResponse s r)

buildArgs :: Show a => [(String,a)] -> String
buildArgs [] = ""
buildArgs args = '?' : (tail $ foldl (\a -> \(s,v) -> a ++ "&" ++ s ++ "=" ++ show v) "" args)

getUserById :: Int -> IO (Either String User)
getUserById i = get ("/users/id/" ++ show i)

getUserByName :: String -> IO (Either String User)
getUserByName n = get ("/users/name/" ++ n)

getProductById :: Int -> IO (Either String Product)
getProductById i = get ("/products/id/" ++ show i)

getProductByEAN :: Integer -> IO (Either String Product)
getProductByEAN n = get ("/products/ean/" ++ show n)

getProductByName :: String -> IO (Either String Product)
getProductByName n = get ("/products/name/" ++ n)

getTransactionById :: Int -> IO (Either String Transaction)
getTransactionById i = get ("/transactions/" ++ show i)

getAllProducts :: IO [Product]
getAllProducts = getAll "/products/"

getAllUsers :: IO [User]
getAllUsers = getAll "/users/"

getAllTransactions :: IO [Transaction]
getAllTransactions = getAll "/transactions/"

addUser :: User -> IO (Either String ())
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
  get (path ++ buildArgs [("max", range), ("offset", offset)])
