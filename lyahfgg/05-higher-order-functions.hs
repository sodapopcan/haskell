-- All functions officially only take one arg.
-- When you define a function with more than one arg, you are technically
-- defining a function that returns a function that possibly returns another
-- function and so on (depending on how many args you have).  This is the reason
-- that type signatures use -> to separate args and return value.
--    max :: (Ord a) => a -> a -> a
-- This is essentially saying max takes an arg "a" and returns a function that
-- takes an arg "a" and returns an "a".
-- So calling:
--    max 1 2
-- is effectively calling:
--    (max 1) 2
-- To partially apply (or curry) a function, all you have to do is not pass all
-- the args.  For example:
--    alwaysCompareWithTen = max 10
-- Now `alwaysCompareWithTen` is a function that takes one arg
--    alwaysCompareWithTen 12    => 12
--    alwaysCompareWithTen 2     => 10
-- A real-world application for this would be, for example, a function that
-- takes a database connection and a query.  You could then create a new
-- function by partially applying just the database connection, then you can run
-- repeated queries to the same database without having to pass the connection
-- each time.
-- To partially apply an infix function, surround it with parens and provide an
-- argument to only one side.
--    divideByTwo = (/2)
-- The next passed argument will be applied to the empty side:
--    divideByTwo 14        => 14.0
-- These are called "sections"
-- As seen before, the only caveat is that (-4) is a convienience syntax for
-- negative numbers, so that syntax is *not* partially applying 4 to the
-- subtract function.  Instead, you can use the named function `subtract'.
-- Here's a function that tells you if a Character is uppercase or not:
isUppercase :: Char -> Bool
isUppercase = (`elem` ['A'..'Z'])

-- Here is a version that takes a string or char
isUpperString :: String -> Bool
isUpperString x = (x !! 0) `elem` ['A'..'Z']

-- This is a function that takes 1) a function that takes any type and 2) any
-- type (that is the same as before) and returns return a value of that type.
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- For example:
--   applyTwice (+3) 2       => 8

-- Here is an implementation of zipWith:
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
