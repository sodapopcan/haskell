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
