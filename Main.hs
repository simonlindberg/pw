-- cabal install aes
-- cabal install cryptohash
import PW.FileHandling
import PW.Generate
import PW.Util

import Control.Exception (bracket_)

import System.Environment (getArgs)
import System.IO (putChar, putStr, putStrLn, hFlush, stdout, stdin, hSetEcho, hGetEcho)

addOption    = ["-a", "-add"]              :: Option
genOption    = ["-g", "-gen", "-generate"] :: Option
readOption   = ["-r", "-read"]             :: Option
createOption = ["-c", "-create"]           :: Option

pwOption     = ["-p", "-pw", "-password"]  :: Option
tagOption    = ["-t", "-tag", "-id"]       :: Option
fileOption   = ["-f","-file"]              :: Option

helpOption   = ["-h","-help"]              :: Option

type Option = [String]

{-
-g -gen -generate 
-a -add
-c -create (length)
-r -read (tag)
-----------------------------------
-f -file [filepath]
-p -pw -password [password]
-t -tag -id -ID [tag]
-----------------------------------
-h -help


generate 12 -> a twelve character long password is written to stdout.
generate    -> a ten character long password is written to stdout.

create -f file -> promted with request for password

add -t tag -p pw -f file                        -> prompts with password request
add -t tag -generate 15 -create -f file         -> prompts with password request for the new file.

-------------------------------
read -t tag -f file  ->  prompts for password and prints the given password (if available)
read -f file      ->  prompts for password and prints all passwords.

-}

defaultPasswordLength = 10

main :: IO ()
main = do
  args <- getArgs
  case getFile args of
    Just file -> fileRelated args file
    Nothing   -> generate args

fileRelated :: [String] -> String -> IO ()
fileRelated args file = createFile >> addPW >> readFile
    where
      createFile :: IO ()
      createFile = do
        case index createOption args of
          Nothing -> return ()
          otherwise -> do
            passphrase <- newPassphrase
            createNewFile file passphrase
      
      addPW :: IO ()
      addPW = case index addOption args of
        Nothing -> return ()
        otherwise -> do
          let maybePW = getArgFromOption pwOption  args
          let tag     = getArgFromOption tagOption args
          let genArg  = getArgFromOption genOption args
          
          realPW <- password maybePW genArg
          passphrase <- getPassphrase
          addPasswordToFile file (deTag tag) realPW passphrase
            where
              deTag :: Maybe String -> String
              deTag (Just tag) = tag
              deTag Nothing    = error $ "Error! No tag, add a tag with '-tag my_tag'"
              
              --         -pw 'password'   -g '10' 
              password :: Maybe String -> Maybe String -> IO String
              password Nothing Nothing = error $ "Error! No password provided! Either add one with '-pw my_password' or generate one with '-generate'."
              password (Just pw) _     = return pw 
              password _ (Just gen)    = genPW args
      
      readFile :: IO ()
      readFile = case index readOption args of
        Nothing -> return ()
        Just i  -> do
        
          let tag = getArgFromOption tagOption args
          passphrase <- getPassphrase
          case tag of
            Just t  -> getTaggedPasswords t passphrase
            Nothing -> getAllPassword passphrase
            where
              getAllPassword :: String -> IO ()
              getAllPassword passphrase = do
                pws <- readPasswordsFromFile file passphrase
                print_ pws

              getTaggedPasswords :: String -> String -> IO ()
              getTaggedPasswords tag passphrase = do
                pws <- getPasswordsFromFile file tag passphrase
                case pws of
                  [] -> putStrLn "No password found."
                  otherwise -> mapM_ putStrLn pws

              print_ :: [(String, String)] -> IO ()
              print_ []         = return ()
              print_ ((a,b):as) = putStrLn (a ++ ": " ++ b) >> (print_ as)


newPassphrase :: IO String
newPassphrase = do 
  p1 <- passphrasePrompt "Choose a master passphrase: "
  p2 <- passphrasePrompt "Confirm the passphrase: "

  if p1 == p2 then return p1 else putStrLn "passwords doesn't equal. Try again.\n" >> newPassphrase

getPassphrase :: IO Passphrase
getPassphrase = passphrasePrompt "Passphrase: "

passphrasePrompt :: String -> IO Passphrase
passphrasePrompt prompt = do
  putStr prompt
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getArgFromOption :: Option -> [String] -> Maybe String
getArgFromOption option args = case index option args of
  Just i  -> checkLength (i + 1) -- Just $ args !! (i + 1)
  Nothing -> Nothing
  where checkLength i = if length args <= i then Nothing else Just $ args !! i

getFile :: [String] -> Maybe String
getFile = getArgFromOption fileOption

genPW :: [String] -> IO String
genPW args = generatePW $ getGenerateLength args
  where
    getGenerateLength :: [String] -> Int
    getGenerateLength args = case getArgFromOption genOption args of
        Nothing  -> defaultPasswordLength
        Just len -> readIt len
          where
            readIt len = case reads len :: [(Int, String)] of
              [(l, "")] -> l
              otherwise -> defaultPasswordLength

generate :: [String] -> IO ()
generate args = case index genOption args of
  Nothing   -> help args
  otherwise -> do
    pw <- genPW args -- Since no file is found, it has to be '-generate'
    putStrLn pw


help :: [String] -> IO ()
help args = case index helpOption args of
  Nothing   -> error "Incorrect parameters (you might just have forgotten to specifiy a file). Try -help."
  otherwise -> putStrLn $ gen ++ add ++ create ++ read ++ file ++ pw ++ tag ++ examples
  where
    gen    = "\'-generate\'\tGenerates a random password. Takes an optional integer as input, the integer determineds the length of the password. The default length is ten.\n\n"
    add    = "\'-add\'\t\tAdds a new password to a specified file. Requires a file, a tag and a password (either specified with -password or generated with -generate).\n\n"
    create = "\'-create\'\tCreates a new file. Requires a specified file.\n\n"
    read   = "\'-read\'\t\tReads passwords from a specified file. Takes an optional tag as input, if given it will only print the password related to it otherwise all passwords will be printed.\n\n"

    file   = "\'-file\'\t\tSpecifies the file to be used.\n\n"
    pw     = "\'-password\'\tSpecifies the password to be used.\n\n"
    tag    = "\'-tag\'\t\tSpecified the tag to be used.\n\n"

    examples = ""