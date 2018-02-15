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
-- Compiler complaints about non-exhaustive patterns mean that you need to add
--    a catch-all clause.  I like the word "exhaustive" because it has my last
--    name in it.
-- You can pattern match on vectors too (the pattern matching being the
--    arguments as vectors):
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x, y) (x2, y2) = (x + x2, y + y2)
-- You should use _ to skip stuff you don't care about:
getSecond :: (a, b, c) -> b
getSecond (_, x, _) = x
-- It's not mandatory, this still works:
getFirst :: (a, b, c) -> a
getFirst (a, b, c) = a
-- But obvs just use underscore!  You are a heathen, afterall.
-- You can pattern match on lists with the : syntax.  For example, to get head
--    and tail, you can do: h:t = [1,2,3,4].  This will bind h to 1 and t to
--    [2,3,4].  It is more common to use x:xs, though.
-- Here is a function that returns the head and tail of a list as a tuple:
getHeadAndTail :: [a] -> (a, [a])
getHeadAndTail (h:t) = (h,t)
-- From the book, here is pattern matching in a list comprehension:
squishTuples :: (Num a) => [(a,a)] -> [a]
squishTuples l = [ a + b | (a, b) <- l]
