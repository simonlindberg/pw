module Main( main ) where

import PW.FileHandling
import PW.Generate
import PW.Util

import Data.Maybe (isNothing, isJust, fromJust)

import System.Console.GetOpt
import System.Environment (getArgs)


data Options = Options {
  toAdd      :: Bool,
  toCreate   :: Bool,
  toGenerate :: Bool,
  file       :: Maybe FilePath,
  genLength  :: Int,
  password   :: Maybe Password,
  toRead     :: Bool,
  tag        :: Maybe Tag,
  passphrase :: Maybe Passphrase
} deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options {
  password   = Nothing,
  toGenerate = False,
  genLength  = 10,
  toCreate   = False,
  toAdd      = False,
  toRead     = False,
  file       = Nothing,
  tag        = Nothing,
  passphrase = Nothing
}

header = "\tUsage: "

optionFlags :: [OptDescr (Options -> IO Options)]
optionFlags = [
  Option ['a'] ["add"]    (NoArg  optionAdd) "Adds a password.",
  Option ['c'] ["create"] (NoArg  optionCreate) "Creates a new file.",
  Option ['f'] ["file"]   (ReqArg optionFile "FILE") "Specifies the file to be used.",
  Option ['g'] ["generate", "gen"] (OptArg optionGen "LENGTH") "Generates a random password, default length 10.",
  Option ['p'] ["password", "pw"]     (ReqArg optionPassword "PASSWORD") "Specifies the password to be used.",
  Option ['r'] ["read"]   (NoArg  optionRead) "Reads passwords from a specified file. If a -tag is given it will only read that otherwise it reads all passwords.",
  Option ['t'] ["tag"]    (ReqArg optionTag "TAG") "Specified the tag to be used."
  ]

optionAdd :: Options -> IO Options
optionAdd opts = return opts {toAdd = True}

optionCreate :: Options -> IO Options
optionCreate opts = return opts {toCreate = True}

optionRead :: Options -> IO Options
optionRead opts = return opts {toRead = True}

optionFile :: String -> Options -> IO Options
optionFile f opts = return opts {file = Just f}

optionTag :: String -> Options -> IO Options
optionTag t opts = return opts {tag = Just t}

optionPassword :: String -> Options -> IO Options
optionPassword pw opts = return opts {password = Just pw}

optionGen :: Maybe String -> Options -> IO Options
optionGen Nothing    opts = return opts {toGenerate = True}
optionGen (Just str) opts = case reads str :: [(Int, String)] of
  [(l, "")] -> return opts {genLength = l, toGenerate = True}
  otherwise -> return opts {toGenerate = True}


main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute optionFlags args of
    (flags, [],  []) -> foldl (>>=) (return defaultOptions) flags >>= (\options -> execute options)
    (_, nonOpts, []) -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,   _,   msgs) -> error $ concat msgs ++ usageInfo header optionFlags

execute :: Options -> IO ()
execute opts
  | toCreate opts   = create opts
  | toAdd opts      = add    opts
  | toRead opts     = read_  opts
  | toGenerate opts = generate opts >>= (\t -> putStrLn t)
  | otherwise       = return ()

create :: Options -> IO ()
create options
  | isNothing $ file options = error $ "no file specified!"
  | otherwise = do
    let (Just f) = file options
    passphrase <- newPassphrase
    createNewFile f passphrase
    execute $ options {toCreate = False, passphrase = (Just passphrase)}

add :: Options -> IO ()
add options
  | isNothing $ file options    = error $ "no file specified!"
  | isNothing $ tag options     = error $ "no tag specified!"
  | (not . anyPassword) options = error $ "no password provided!"
  | isJust $ passphrase options = addPW options
  | otherwise = do
    passphrase <- getPassphrase
    addPW $ options {passphrase = (Just passphrase)}

addPW :: Options -> IO ()
addPW options = do
  let (Just t) = tag options
  let (Just f) = file options
  let (Just p) = passphrase options
  pw <- getPassword options
  addPasswordToFile f t pw p
  execute $ options {toAdd = False}

getPassword :: Options -> IO String
getPassword options 
  | toGenerate options = generate options
  | isJust $ password options = (return . fromJust . password) options
  | otherwise = error $ "no password specified!"

anyPassword :: Options -> Bool
anyPassword options = (isJust . password) options || toGenerate options

read_ :: Options -> IO ()
read_ options
  | isNothing $  file options   = error $ "no file specified!"
  | isJust $ passphrase options = readPWs options
  | otherwise = do
    passphrase <- getPassphrase
    readPWs $ options {passphrase = (Just passphrase)}

readPWs :: Options -> IO ()
readPWs options = do
  let (Just f) = file options
  let (Just p) = passphrase options
  case tag options of
    Nothing -> readAll f p
    Just t  -> readTag t f p

readAll :: FilePath -> Passphrase -> IO ()
readAll f p = do
 pws <- readPasswordsFromFile f p
 print_ pws
  where
    print_ []          = return ()
    print_ ((t,p):pws) = putStrLn (t ++ ": " ++ p) >> (print_ pws)

readTag :: Tag -> FilePath -> Passphrase -> IO ()
readTag t f p = do
  pws <- getPasswordsFromFile f t p
  mapM_ putStrLn pws

generate :: Options -> IO String
generate = generatePW . genLength