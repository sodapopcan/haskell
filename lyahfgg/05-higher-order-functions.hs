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
--    alwaysCompareToTen = max 10
-- Now `alwaysCompareToOne` is a function that takes one arg
--    alwaysCompareToOne 12    => 12
--    alwaysCompareToOne 2     => 10
-- A realworld application for this would be, for example, a function that takes
-- a database connection and a query.  You could then create a new function by
-- partially applying just the database connection, then you can run repeated
-- queries to the same database without having to pass the connection each time.

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
