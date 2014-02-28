module Tests.TestGenerate (test) where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import PW.Generate

-- Defining a smaller version of Int. 2 - 100, 1 is too small (too likly that two passwords will be equal).
newtype SmallInt = SmallInt Int deriving (Show)

instance Arbitrary SmallInt where
  arbitrary = do 
    large <- arbitrary
    return $ SmallInt $ 2 + large `mod` 100

-- Property 1: Password should be the correct size.
prop1 :: SmallInt -> Property
prop1 (SmallInt len) = monadicIO $ do
  pw <- run $ generatePW len
  assert $ len == length pw


-- Property 2: Two password should not be equal.
prop2 :: SmallInt -> Property
prop2 (SmallInt len) = monadicIO $ do
  pw1 <- run $ generatePW len
  pw2 <- run $ generatePW len
  assert $ pw1 /= pw2

test :: IO ()
test = do putStrLn "\n--- Testing the password generation module"
          putStrLn "\nProperty 1: Password should be the correct size."
          quickCheckWith stdArgs {maxSuccess = 5000} prop1
          putStrLn "\nProperty 2: Two password should not be equal."
          quickCheckWith stdArgs {maxSuccess = 5000} prop2
