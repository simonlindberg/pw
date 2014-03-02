module PW.Util (strictRead, index, newPassphrase, getPassphrase) where

import Data.List (elemIndex)

import qualified Data.ByteString as B

import Control.Exception (bracket_)

import System.IO (putChar, putStr, putStrLn, hFlush, stdout, stdin, hSetEcho, hGetEcho)

-- Reads the contents of a given file strict
strictRead :: FilePath -> IO B.ByteString
strictRead f = B.readFile f >>= \s -> B.length s `seq` return s

-- Gives the index of the first item from the first list that exists in the second.
index :: Eq a => [a] -> [a] -> Maybe Int
index []     _    = Nothing
index (x:xs) list = case elemIndex x list of 
  Nothing -> index xs list
  value   -> value

newPassphrase :: IO String
newPassphrase = do 
  p1 <- passphrasePrompt "Choose a master passphrase: "
  p2 <- passphrasePrompt "Confirm the passphrase: "
  if p1 == p2
  then return p1
  else putStrLn "passphrases isn't equal. Try again.\n" >> newPassphrase

getPassphrase :: IO String
getPassphrase = passphrasePrompt "Passphrase: "

passphrasePrompt :: String -> IO String
passphrasePrompt prompt = do
  putStr prompt
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass
    where 
      withEcho :: Bool -> IO a -> IO a
      withEcho echo action = do
        old <- hGetEcho stdin
        bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action