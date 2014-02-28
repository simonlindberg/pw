module Tests.TestFileHandling (test) where

import PW.Util
import PW.FileHandling
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Arbitrary
import System.IO
import System.Directory
import System.Random
import Data.Char
import Control.Exception (catch, Exception)


file :: FilePath
file = "test.pw"

newtype Word = Word String deriving (Show)

instance Arbitrary Word where
  arbitrary = do
    chars <- arbitrary
    return $ Word $ fix chars
      where
        fix []   = "a"
        fix list = map replaceSpace list
        replaceSpace a
          | isSpace a = 'a'
          | otherwise = a

-- Property 1: New files should be created with the correct verifier.
prop1 :: Word -> Property
prop1 (Word pw) = monadicIO $ do
  run $ createNewFile file pw
  empty <- run $ getPasswordsFromFile file "" pw
  run $ cleanIO
  assert $ empty == []

-- Property 2: Added password should be returned correctly.
prop2 :: Word -> Word -> Word -> Property
prop2 (Word tag) (Word pw) (Word passphrase) = monadicIO $ do
  run $ createNewFile file passphrase
  run $ addPasswordToFile file tag pw passphrase
  pw_get <- run $ getPasswordsFromFile file tag passphrase
  run $ cleanIO
  assert $ [pw] == pw_get


-- Property 3: Removed password should not be removed.
prop3 :: Word -> Word -> Word -> Property
prop3 (Word tag) (Word pw) (Word passphrase) = monadicIO $ do
  run $ createNewFile file passphrase
  run $ addPasswordToFile file tag pw passphrase
  run $ removePasswordFromFile file tag passphrase
  pw_get <- run $ getPasswordsFromFile file tag passphrase
  run $ cleanIO
  assert $ pw_get == []

-- Property 4: File should be unreadable with wrong password
prop4 :: Word -> Word -> Word -> Word -> Property
prop4 (Word tag) (Word pw) (Word passphrase) (Word fake_passphrase) = monadicIO $ do
  if passphrase == fake_passphrase
    then assert True
    else do
      run $ createNewFile file passphrase
      run $ addPasswordToFile file tag pw passphrase
      pw_get <- run $ catch (getPasswordsFromFile file tag fake_passphrase) handle
      run $ cleanIO
      assert $ pw_get == []
        where 
          handle :: BadPassword -> IO [String]
          handle _ = return []

-- Property 5: Reading the file should yeild a correct info.
prop5 :: NonEmptyList Word -> NonEmptyList Word -> Word -> Property
prop5 (NonEmpty tagWords) (NonEmpty pwWords) (Word passphrase) = monadicIO $ do
  let len  = min (length tagWords) (length pwWords)
  let tags = map unword $ take len tagWords
  let pws  = map unword $ take len pwWords

  run $ createNewFile file passphrase
  run $ addAll tags pws
  content <- run $ readPasswordsFromFile file passphrase
  run $ cleanIO
  assert $ expected tags pws == content
    where      
      addAll []     []     = return ()
      addAll (t:ts) (p:ps) = addPasswordToFile file t p passphrase >> addAll ts ps
      
      expected []     []     = []
      expected (t:ts) (p:ps) = (t, p) : expected ts ps

-- Property 6: Reading a tag should return all passwords with that tag.
prop6 :: Word -> Word -> [Word] -> Property
prop6 (Word tag) (Word passphrase) pwWords = monadicIO $ do
  let pws = map unword pwWords
  run $ createNewFile file passphrase
  run $ addAll pws 
  content <- run $ getPasswordsFromFile file tag passphrase
  run $ cleanIO
  assert $ pws == content
    where
      addAll []     = return ()
      addAll (p:ps) = addPasswordToFile file tag p passphrase >> addAll ps

unword :: Word -> String
unword (Word a) = a

test :: IO ()
test = do putStrLn "\n--- Testing the file handling module"
          putStrLn "\nProperty 1: New files should be created with the correct description."
          quickCheck prop1
          cleanIO
          putStrLn "\nProperty 2: Added password should be returned correctly."
          quickCheck prop2
          cleanIO
          putStrLn "\nProperty 3: Removed password should not be removed."
          quickCheck prop3
          cleanIO
          putStrLn "\nProperty 4: File should be unreadable with wrong password"
          quickCheck prop4
          cleanIO
          putStrLn "\nProprty 5: Reading the file should yeild a correct info."
          quickCheck prop5
          putStrLn "\nProperty 6: Reading a tag should return all passwords with that tag."
          quickCheck prop6
          
cleanIO = do
  exists <- doesFileExist file
  if exists then removeFile file else return ()

clean = run $ cleanIO 