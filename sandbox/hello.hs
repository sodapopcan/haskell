import System.IO
import Control.Monad

main = do
  putStrLn "What's your name?"
  putStr "> "
  name <- getLine
  when (name /= "q") $ do
    case name of
      "?" ->
        putStrLn "" >>
        putStrLn "  q   Quit" >>
        putStrLn "  ?   Help" >>
        putStrLn ""
      _ ->
        putStrLn ("Oh hi there, " ++ name)
    main
