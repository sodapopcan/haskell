-- Haskell is statically typed, but we knew that already
-- Types can be inferred, but it's better to write a typedef except for really
--    simple functions (maybe even for them too)
-- 'a' :: Char
-- True :: Bool
-- "Hello" :: [Char] (tho you can define as String)
-- (True, 'a') :: (Bool, Char)
-- Return value is specified after the `->`
removeNonUppercase :: String -> String
removeNonUppercase str = [ c | c <- str, c `elem` ['A'..'Z'] ]
-- Multiple args are separated by `->` as well.  Apparently there is a good
--    for there not being different syntax to separate args and return vals but
--    we aren't being told why yet.
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
-- Int is reg-sized integer (based on machine) and Integer can hold massive
--    numbers
-- Type variables allow different types to be passed to functions
-- fst :: (a, b) -> a
-- ie, `fst` takes a size 2 tuple containing any two types and returns the first
--    element.  `snd` would be (a, b) -> b
-- Typeclass add a constraint to what may be passed.  For example, if a function
--    has take two varialby typed variables, you can specify that they must be
--    of the same type.  For example, the equality operator:
-- (==) :: (Eq a) => a -> a -> Bool
-- This means that while both a's must be of the same type (whatever that type
--    may be:  1 == 1 or "fry" == "zoidberg" for example
--    1 == "andrew" does not work
-- (+) :: Num a => a -> a -> a means a can be of any Num type
-- All types, except IO, are a member of Eq
-- Functions with Eq are going to use == or /= somewhere in their definition
compareThing :: (Eq a) => a -> a -> String
compareThing a b =
  if a == b
     then "yes"
     else "nope"
-- compareThing 1 3 => "nope"
-- compareThing 'h' 'h' => "yes"
-- compareThing 4 4 => "yes"
-- Members of the Show typeclass can be represented as strings.  Everything but
--    functions belong to show.  Use the `show` function to convert:
--    show 1 becomes "1"
-- `read` takes a string and converts it into a member of Read.
--    read "True" || False becomes True, for example
-- Haskell is inferring what to convert the type to based on the rest of the
--    expression.  If you want to use read by itself, you need to say what type
--    you want: read "1" :: Int is 1
--    read "[1,2,3,4]" ++ [5,6] becomes [1,2,3,4,5,6]
