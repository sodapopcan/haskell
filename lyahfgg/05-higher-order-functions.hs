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
-- Alternatively:
--    divideTenBy = (10/)
--    divideTenBy 2         => 21.0
-- These are called "sections"
-- As seen before, the only caveat is that (-4) is a convenience syntax for
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
-- zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- A type signature with a -> b -> c means each var may be of any type and they
-- don't have to match, but it's ok if they do!  If it were a -> a -> a then
-- they would all have to match.

-- There are examples in the book on how to use zipWith'.  The last one is this:
--    zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-- The first argument had me scratching my head until I realized it's creating
-- a curried function inline and I was like, "Ohhhh shit!"  That's pretty cool.
-- So yeah, zipWith' (*) creates a function that takes two lists and zips them
-- by multiplying each pair.

-- flip is in the standard lib... it takes two functions and flips their
-- arguments around... that's interesting...
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x
-- And ya, the parens on the right aren't necessary since `->' is right
-- associative anyway so this is equivalent:
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x
-- I kinda don't care about this right now...

-- Now we get into maps and filters.  I know this shit!  I'm going to try and
-- guess the examples given and then do my own implementations.
plusOneMap :: (Num a) => [a] -> [a]
plusOneMap xs = map (+1) xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs

-- Woo!!!  I guessed right for the implementation of map!  Filter not so much...
-- not even going to make a record of my attempt since it wasn't even complete.
-- But here is a function I wrote to filter even numbers out of a list:
filterEvens :: [Int] -> [Int]
filterEvens xs = filter (\x -> x `mod` 2 /= 0) xs

-- And here is a reimplementation of filter:
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : filter' f xs
  | otherwise = filter' f xs

-- annnnd quicksort using filter instead of list comprehension:
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let smaller = qsort $ filter (<x) xs
      bigger  = qsort $ filter (>x) xs
  in smaller ++ [x] ++ bigger

-- Find the largest number under some number that's divisible by some other number
largestDivisible :: (Integral a) => a -> a -> a
largestDivisible haystack needle =
  head $ filter p [haystack, (haystack - 1)..]
    where p x = x `mod` needle == 0

-- takeWhile grabs elements of a list until the predicate returns false
firstWord :: String -> String
firstWord xs = takeWhile (/= ' ') xs

-- These math problems are boring me.  I read them (and not for the first time)
-- buit I'm skipping ahead.

-- Lambdas
-- This just explains lambdas which I've already been using

-- Folding
-- So foldl and friends are basically reduce.  It takes the pattern of using
-- x:xs and defining a special case empty list clause and abstracts it.
-- Again, it's like reduce.
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs
-- A simpler implementation would be
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0
-- Here we partially apply foldl l so sum'' is a function that takes a list
-- still.
-- An implementation of elem that confused me but I get it now:
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if y == x then True else acc) False ys
-- This is obviously inefficient as it walks the entire list o matter what.
-- foldr is like foldl but starts folding from the right.  This essentially
-- useful for performance.
-- For example, here is an implementation of map using foldr:
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- It could be done with foldl like this:
--    map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
-- But, as we learned previously, : is way cheaper than ++, especially on big
-- lists
-- There is also foldl1 and foldr1 (what names!).  They use the first element of
-- the list as the starting value of the accumulator.
-- Next there is scanl and scanr
-- These return a list of all values of the accumulator throughout the
-- computation:
--    scanl (+) 0 [1,2,3,4,5]          => [0,1,3,6,10,15]
--    scanl1 (+) [1,2,3,4,5]           => [1,3,6,10,15]
-- More examples I'm gonna skip cos they are boring me for now.
-- Next it talks about function application with $ which I've already been
-- doing.  It's interesting of course that it is a function itself (as
-- everything basically is).  It's basically a function that takes a function
-- and a value and applies the value to function but it's right associative so
-- it can replace brackets.
--
-- Function Composition
-- The `.' is used for function composition.  If the return value of a function
-- has the same type as another, they can be composed.
-- From the book:
--    fn x = ceiling (negate (tan (cos (max 50 x))))  
-- is the same as:
--    fn x = ceiling . negate . tan . cos . max 50
-- I'm gonna jump ahead now
