module Tests.TestCipher (test) where

import Test.QuickCheck
import PW.Cipher
import qualified Data.ByteString.Char8 as B

-- Property 1: Any string should be the same after it has gone through encryption and decryption with any password.
prop1 :: NonEmptyList Char -> NonEmptyList Char -> Bool
prop1 (NonEmpty pw) (NonEmpty plaintext) = plaintext == ((decrypt pw) . (encrypt pw)) plaintext

-- Property 2: Any string should not look the same after encryption (might actually fail for short strings).
prop2 :: NonEmptyList Char -> NonEmptyList Char -> NonEmptyList Char -> Bool
prop2 (NonEmpty pw) (NonEmpty p1) (NonEmpty p2) = plaintext /= (B.unpack $ encrypt pw plaintext)
	where plaintext = p1 ++ p2 -- to avoid 1 element short strings.

-- Property 3: The length of the encrypted string should be the same.
prop3 :: NonEmptyList Char -> NonEmptyList Char -> Bool
prop3 (NonEmpty pw) (NonEmpty c) = length c == (B.length $ encrypt pw c)

-- Property 4: The cipher text from two encryptions with diffrent passwords should not be equal.
prop4 :: NonEmptyList Char -> NonEmptyList Char -> Bool
prop4 (NonEmpty pw1) (NonEmpty pw2) = if pw1 == pw2 then True else encrypt pw1 plaintext /= encrypt pw2 plaintext
	where plaintext = "this will be the plain text for the test we are doing" 

test :: IO ()
test = do putStrLn "\n--- Testing the cipher module"
          putStrLn "\nProperty 1: Any string should be the same after it has gone through encryption and decryption with any password."
          quickCheckWith stdArgs {maxSuccess = 2000} prop1
          putStrLn "\nProperty 2: Any string should not look the same after encryption."
          quickCheckWith stdArgs {maxSuccess = 2000} prop2
          putStrLn "\nProperty 3: The length of the encrypted string should be the same."
          quickCheckWith stdArgs {maxSuccess = 2000} prop3
          putStrLn "\nProperty 4: The cipher text from two encryptions with diffrent passwords should not be equal."
          quickCheckWith stdArgs {maxSuccess = 2000} prop4