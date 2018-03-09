module Convoluted
  ( alwaysTheSameThing
  , convolutedUpcase
  , convolutedDowncase
  ) where

import Data.Char

alwaysTheSameThing :: IO ()
alwaysTheSameThing = putStrLn "I am thing"

convolutedUpcase :: String -> String
convolutedUpcase = convolutedCase (-) ['a'..'z']

convolutedDowncase :: String -> String
convolutedDowncase = convolutedCase (+) ['A'..'Z']

convolutedCase :: (Int -> Int -> Int) -> String -> String -> String
convolutedCase f xs x =
  map g x
  where g = \y -> if y `elem` xs
                     then chr $ f (ord y) 32
                     else y
