import System.IO
import Control.Monad

import Convoluted (convolutedUpcase)

main = do
  putStrLn "What's your name?"
  putStr "> "
  name <- getLine
  when (name /= "q") $ do
    case name of
      "?" -> do
        putStrLn ""
        putStrLn "  q   Quit"
        putStrLn "  ?   Help"
        putStrLn ""
      _ ->
        putStrLn ("Oh hi there, " ++ convolutedUpcase name ++ "!!!")
    main
