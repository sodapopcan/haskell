import System.IO

main = do
  putStrLn "What's your name?"
  putStr "> "
  name <- getLine
  case name of
       "\\q" ->
         putStrLn "bye" >> 
         return ()
       _ ->
         putStrLn ("Oh hi there, " ++ name) >> 
         main
