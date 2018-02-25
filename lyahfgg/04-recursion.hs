-- Recursion
--
-- I've spent a long time trying to grok this implementation of maximum.
maxList :: (Ord a) => [a] -> a
maxList [] = error "No maximum of an empty list."
maxList [x] = x
maxList (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maxList xs
-- ^ I was always fine with recursion, and this is insanely expressive but it's
--    destroying my mind :\

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x 

-- This implementation of take illustrates how a guard with no otherwise means
--    that if the guard is false, it will fall through to the next pattern
--    definition.  That's neat.
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

-- Did this one without looking... look at me.  Woo.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
  | y == x = True
  | otherwise = elem' y xs

-- Quicksort... has a photo of Quickman, woo!
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let smaller = qsort [ a | a <- xs, a <= x ]
      larger  = qsort [ a | a <- xs, a > x ]
  in smaller ++ [x] ++ larger
