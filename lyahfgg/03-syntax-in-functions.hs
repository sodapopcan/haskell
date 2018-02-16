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
-- Oh yeah, you can also bind against any number of elements if you keep using
--    the : operator:
getFirstAndSecondAndRest :: (Num a) => [a] -> (a, a, [a])
getFirstAndSecondAndRest (h1:h2:t) = (h1, h2, t)
-- This example from the book tells us what *some* of the elements of a list
--    are:
tell :: (Show a) => [a] -> String
tell [] = "List is empty, brah"
tell (x:[]) = "List has only one element, brah.  C'mon, brah.  I want more than just a " ++ show x
tell (x:y:_) = "Ok, now you got two things, " ++ show x ++ " and " ++ show y ++ ".  Now we're getting somewhere, brah.  Yea brah!"
-- So I don't fully understand bracket use yet, but list syntax is actually
--    syntactic sugar.  So (1:[]) is the same as [1] (and actually, on the repl
--    at least, you don't need the bracket).
-- Here's a re-implementation of the length function with pattern matching and
--    recursion;
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
-- So this is saying "If an empty list, then return 0.  Without an empty list,
--    disgard the first element and return the length' of the list + 1.
--    This is essentially the same as replacing every element in an array with
--    1 and summing the array.  Oh, here is that implementation!:
lengthAgain :: (Num b) => [a] -> b
lengthAgain x = sum $ map (\_ -> 1) x
-- It doesn't even need pattern matching!
-- Oh yeah, I read ahead an `$` is a way of binding tighter, later.  IE, it gets
--    rid of parens:  `sum (map (+1) [1])` is the same as `sum $ map (+1) [1]`
-- Here is the book's example of a reimplementation of sum:
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
-- It's the same idea as length'.  If we get an empty list, just say `0`,
--    otherwise we patten match on the head and the tail we are passed, then we
--    add the head to result of the recursive call.
--
-- Ok, so "as patterns" are kinda blowing my mind
-- So far, I never realized that this was a thing:
--    x:y:z = [1,2,3]
--    Ok, so I knew about that and then x = 1, y = 2, and z = 3
--    BUT, I didn't realize that x:y:z is a valid "identifier" (probably the
--    wrong thing to call it).  If you type this out `x:y:z` in the repl, it
--    will return [1,2,3]!!!!
-- SOOOO you can use the "all patterns" syntax--which is @--in pattern matching
--    to get an easy reference to the original thing:
printTheFirstLetter :: String -> String
printTheFirstLetter str@(h:t) =
  "So you gave me '" ++ str ++ ".' "
  ++ " The first letter is " ++ [h]
  ++ " and the rest is " ++ t
