module PW.Util (strictRead, index, lookupAll) where

import Data.List (elemIndex)

import qualified Data.ByteString as B

-- Reads the contents of a given file strict
strictRead :: FilePath -> IO B.ByteString
strictRead f = B.readFile f >>= \s -> B.length s `seq` return s

-- Gives the index of the first item from the first list that exists in the second.
index :: Eq a => [a] -> [a] -> Maybe Int
index []     _    = Nothing
index (x:xs) list = case elemIndex x list of 
  Nothing -> index xs list
  value   -> value

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll x pairs = [b | (a, b) <- pairs, a == x]