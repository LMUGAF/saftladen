import System.Environment (getArgs)

import REST
import Types
import Interface

{-
main = do args <- getArgs
          user <- getUserById $ read $ head args
          case user of
            Left err -> putStrLn err
            Right ps -> print ps
-}

main = run
