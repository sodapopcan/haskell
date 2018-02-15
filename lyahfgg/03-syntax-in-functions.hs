-- Haskell has pattern matching which I know from Elixir and Clojure.
sayHello :: String -> String
sayHello "Andrew" = "I guess you could say hi to yourself."
sayHello name = "Oh hi there, " ++ name ++ "!"
-- sayHello "Andrew"
-- "I guess you could sayi hi to yourself."
-- sayHello "Martha"
-- "Oh hi there, Martha!"
-- Pattern matching functions falls through the order they are defined
-- This isn't reall pattern matching but got here when trying to match on types
toInt :: (RealFrac a) => a -> Int
toInt x = round x
-- So yeah, recursion:
fac :: Integral a => a -> a
fac 0 = 1
fac n = n * fac (n - 1)
-- Uses pattern matching to take care of the 0 case so we don't have to check
--    for it like in other languages
-- 
