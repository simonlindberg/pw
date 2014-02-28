module PW.Generate (generatePW) where

import System.Random

generatePW :: Int -> IO String
generatePW len = pick len
  where
    pick :: Int -> IO String
    pick 0 = do return ""
    pick i = do 
      rest <- pick (i - 1)
      p    <- picked
      return (p : rest)
      where
        picked :: IO Char
        picked = do 
          r <- randomRIO (0, (length ref) - 1)
          return (ref !! r)
    
    ref :: String
    ref = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$!@#%&/\\()[]{}=?+^*'-_.:,;<>~"