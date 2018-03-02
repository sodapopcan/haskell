import System.IO

main = do
  putStrLn "What's your name?"
  putStr "> "
  name <- getLine
  if name == "quit"
     then putStrLn "bye"
     else putStrLn $ "Oh hi there, "  ++ name
  if name /= "quit"
     then main
     else return ()
