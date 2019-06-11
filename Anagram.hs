module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toUpper)

anagramsFor :: String -> [String] -> [String]
anagramsFor s [x]
  | compareWord s x = [x]
  | otherwise = []
anagramsFor s (x:xs) 
  | compareWord s x = x : anagramsFor s xs
  | otherwise = anagramsFor s xs

compareWord :: String -> String -> Bool
compareWord a b 
    | upper a == upper b = False
    | (sort $ upper a) == (sort $ upper b) = True
    | otherwise = False
    where
        upper :: [Char] -> [Char]
        upper = map toUpper 

