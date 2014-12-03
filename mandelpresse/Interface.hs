{-# LANGUAGE OverloadedStrings #-}
module Interface where

import REST
import Types

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Default
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Graphics.Vty.Widgets.All
import Prelude hiding (take)

type EAN = Integer

data InputType = InputUID Integer
               | InputUser String
               | InputEAN EAN
               | InputDeposit Integer
               | InputLogout
                 deriving Show

inputParser :: Parser InputType
inputParser =
      (liftM (InputDeposit) ("deposit" *> skipSpace *> decimal))
  <|> (liftM (InputUser . T.unpack) ("user" *> skipSpace *> takeText))
  <|> (liftM (InputEAN . read . T.unpack) ((take 13) <|> (take 8)))
  <|> (liftM (InputUID . read) (many digit))
  <|> ("deposit" *> return InputLogout)

parseInput :: T.Text -> Either String InputType
parseInput = parseOnly (inputParser <* endOfInput)

--loginUID :: Integer -> IO ()
loginUID = undefined

--loginUser :: String -> IO ()
loginUser = undefined

--showProductInfo :: EAN -> IO ()
showProductInfo = undefined

mainScreen = do
  e1 <- editWidget
  msg <- plainText "        saftladen / mandelpresse\n\
                    \            version 0.1\n\
                    \\n\
                    \type your number or scan your login code\n\
                    \\n\
                    \        press v to list products\n\n"
  e <- vCentered =<< (hCentered msg) <--> (hCentered =<< hFixed 4 e1)

  fg <- newFocusGroup
  addToFocusGroup fg e1

  onActivate e1 (\this -> do
                   input <- getEditText this
                   case parseInput input of
                     (Left e)              -> return ()
                     (Right (InputUID id)) -> loginUID id
                     (Right (InputUser s)) -> loginUser s
                     (Right (InputEAN  p)) -> showProductInfo p
                     otherwise             -> return ()
                   )

  return (e,fg)

productsScreen = do
  products <- getAllProducts
  lst <- newList 1
  forM_ products (\p -> addToList lst p =<< (plainText (p_name p)))

  fg <- newFocusGroup
  addToFocusGroup fg lst

  fg `onKeyPressed` (\_ key _ -> if key == KChar 'q'
                                    then shutdownUi >> return True
                                    else return False)

  return (lst,fg)

run :: IO ()
run = do
  (mui,mfg) <- mainScreen
  (pui,pfg) <- productsScreen

  c <- newCollection
  main     <- addToCollection c mui mfg
  products <- addToCollection c pui pfg

  mfg `onKeyPressed` (\_ key _ -> if key == KChar 'v'
                                     then products >> return True
                                     else return False)

  runUi c defaultContext
