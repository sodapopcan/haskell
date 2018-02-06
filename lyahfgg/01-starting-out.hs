-- This book didn't tell me how to do comments right away... that's cool, tho.
--
-- -2 * 3 works but 3 * (-2) needs to be written like that.
-- And int divded by an int can return a float ( 5 / 2 = 2.5 )
-- Booleans are True and False
-- Equals is == and not equals is /=
-- You cannot mix types (No 5 + "andrew")
-- ...except that you can since there's obviously some sort of typw interface
--    kinda thing as above with int/int=float... and 5+5.0 also works
-- Everything is a function (no surprises there) and while it doesn't tell you
--    how yet, there is a mechnism for defining infix functions
--    (ie, "*" in "3 * 5)
-- Functions call and arguments are all space delimited: func arg1 arg2
--    succ 9    --> 19
--    min 8 2   --> 2
-- Prefix functions (like above) may be converted to infix functions with
--    backticks:
--    div 50 10   --> 5
--    50 `div` 10 --> 5
--    That's pretty cool tho not comfortable with the choice of backticks yet
-- Function calls have the highest precedent!
--    func1 1 + func2 2 is the same as (func1 1) + (func2 2)
sayHi name = "Hi there, " ++ name
-- I tried calling (applying) this function right after the declaration--it
--    didn't like that.
-- sayHi "Andrew"
doubleMe x = x * 2
doubleUs x y = doubleMe x + doubleMe y
-- definition order doesn't matter
doubleSmallNumber x = if x > 100
                         then x
                         else x * 2
-- Skipping added and learning about overloading or whatever Haskell calls it
doubleSmallNumber' x | x > 100 = x
                     | otherwise = doubleMe x
-- ' can be used anywhere in a function name, usually used at the end to denote
--    a strict (non lazy) version of a function, or a slightly modified verion
--    of a function or variable (like `prime` in Pony lang, I guess)
-- Functions that don't take params are called "definitions" or "names"
--
-- Lists are homogenous--they can only store data of the same type
-- Lists of chars become strings (ie, ' vs " is like in C)
--    ['a','n','d','r','e','w'] = "andrew"
--    ie, Strings are Lists
-- Append to lists with ++ ( [1,2,3,4] ++ [5,6,7,8] = [1,2,3,4,5,6,7,8] )
-- When appending, Haskell has to walk through the entire list on the left hand
--    side which is a problem with mammoth lists, not so much with small ones
-- Predend is instantaneous: ( "A" : "NDREW" = "ANDREW" )
-- 1:[2,3,4] is syntactic sugar for 1:2:3:4:[] (you can't do [1]:[2,3]
-- Get an index from a list with !! ( [1,2,3] !! 1 = 2)
-- Nested lists may be of different lengths but still not different types
-- Lists can be compared with > < == >= <= starting from the heads and moving
--    right.  They don't have to be of the same length.  I can't, off the top of
--    my head, think of when to use this so I look forward to that.
-- head and tail do what they do elixir (the first element as a single value,
--    get the remaining elements as a list)
-- last and init do the opposite of head and tail respectively
-- Can't get the head of an empty list!  ERROR
-- length, null, take, drop, reverse, maximum, minimum, sum, product, elem are
--    all functions to use with lists... elem is like "in"
--    4 `elem` [1,2,4,2,3] is True