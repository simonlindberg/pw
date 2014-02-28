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
  nothing <- run $ getPasswordFromFile file "" pw
  run $ cleanIO
  assert $ nothing == Nothing

-- Property 2: Added password should be returned correctly.
prop2 :: Word -> Word -> Word -> Property
prop2 (Word tag) (Word pw) (Word file_pw) = monadicIO $ do
  run $ createNewFile file file_pw
  run $ addPasswordToFile file tag pw file_pw
  pw_get <- run $ getPasswordFromFile file tag file_pw
  run $ cleanIO
  assert $ (Just pw) == pw_get


-- Property 3: Removed password should not be removed.
prop3 :: Word -> Word -> Word -> Property
prop3 (Word tag) (Word pw) (Word file_pw) = monadicIO $ do
  run $ createNewFile file file_pw
  run $ addPasswordToFile file tag pw file_pw
  run $ removePasswordFromFile file tag file_pw 
  pw_get <- run $ getPasswordFromFile file tag file_pw
  run $ cleanIO
  assert $ pw_get == Nothing

-- Property 4: File should be unreadable with wrong password
prop4 :: Word -> Word -> Word -> Word -> Property
prop4 (Word tag) (Word pw) (Word file_pw) (Word fake_pw) = monadicIO $ do
  if file_pw == fake_pw
    then assert True
    else do
      run $ createNewFile file file_pw
      run $ addPasswordToFile file tag pw file_pw
      pw_get <- run $ catch (getPasswordFromFile file tag fake_pw) handle
      run $ cleanIO
      assert $ pw_get == Nothing
        where 
          handle :: BadPassword -> IO (Maybe String)
          handle _ = return Nothing

-- Proprty 5: Reading the file should yeild a correct info.
prop5 :: NonEmptyList Word -> NonEmptyList Word -> Word -> Property
prop5 (NonEmpty tagWords) (NonEmpty pwWords) (Word file_pw) = monadicIO $ do
  let len  = min (length tagWords) (length pwWords)
  let tags = map unword $ take len tagWords
  let pws  = map unword $ take len pwWords

  run $ createNewFile file file_pw
  run $ addAll tags pws
  content <- run $ readPasswordsFromFile file file_pw
  run $ cleanIO
  assert $ expected tags pws == content
    where
      unword (Word a) = a
      
      addAll []     []     = return ()
      addAll (t:ts) (p:ps) = addPasswordToFile file t p file_pw >> addAll ts ps
      
      expected []     []     = []
      expected (t:ts) (p:ps) = (t, p) : expected ts ps

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
          
cleanIO = do
  exists <- doesFileExist file
  if exists then removeFile file else return ()

clean = run $ cleanIO 