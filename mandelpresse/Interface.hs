{-# LANGUAGE OverloadedStrings #-}
module Interface where

import REST
import Types

import Control.Monad
import Data.Default
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Graphics.Vty.Widgets.All

mainScreen = do
  e1 <- hFixed 4 =<< editWidget
  msg <- plainText "        saftladen / mandelpresse\n\
                    \            version 0.1\n\
                    \\n\
                    \type your number or scan your login code\n\
                    \\n\
                    \        press v to list products\n\n"
  e <- vCentered =<< (hCentered msg) <--> (hCentered e1)

  fg <- newFocusGroup
  addToFocusGroup fg e1

  -- onActivate

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
