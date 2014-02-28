module Tests.TestUtil (test) where

import PW.Util
import Data.List
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Random (randomRIO)

-- Proprty 1: Basic property of the function index.
prop1 :: NonEmptyList Int -> Bool
prop1 (NonEmpty xs) = index [] xs == Nothing && case length xs of
  1 -> index [head xs] xs == Just 0
  l -> index [last xs] xs == Just (head $ elemIndices (xs !! (l - 1)) xs)

test :: IO ()
test = do putStrLn "\n--- Testing Utilities module"
          putStrLn "\nProprty 1: Basic property of the function index."
          quickCheckWith stdArgs {maxSuccess = 5000} prop1