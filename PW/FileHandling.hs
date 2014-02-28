{-# LANGUAGE DeriveDataTypeable #-}

module PW.FileHandling
  (BadPassword,
  createNewFile,
  addPasswordToFile,
  getPasswordsFromFile,
  readPasswordsFromFile,
  removePasswordFromFile
  )
  where

import PW.Util
import PW.Cipher
import System.Directory
import Control.Exception
import Data.Typeable.Internal
import qualified Crypto.Hash.SHA512 as SHA512 -- cabal install cryptohash
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Internal (ByteString(PS))
import System.Entropy (getEntropy)

{-
The files follows the following format:
plainVerifier 0 - 63
verifier      64 - 127
passwords     128 - ...

where a password is defined as:
  tag_length  0
  pw_length   1
  tag         2 - (2 + tag_length)
  pw          (3 + tag_length) - (3 + tag_length + pw_length)


kanske sen!

plainVerifier 0 - 63
verifier      64 - 127
mac           128 - 191 

tag_length  192
pw_length   193
tag         194 - (194 + tag_length)
pw          (194 + tag_length) - (194 + tag_length + pw_length)

-}


type Tag          = String
type Password     = String

data FileExist    = FileExist    String deriving (Show, Typeable)
data FileNotExist = FileNotExist String deriving (Show, Typeable)
data BadPassword  = BadPassword  String deriving (Show, Typeable)

instance Exception FileExist
instance Exception FileNotExist
instance Exception BadPassword

-- Verifier length.
vLength :: Int
vLength = 64

checkNotExists :: FilePath -> IO ()
checkNotExists filename = do 
  exists <- doesFileExist filename
  if exists
    then throwIO $ FileExist $ "Error! File already exists: '" ++ filename ++ "'" 
    else return ()

checkExists :: FilePath -> IO ()
checkExists filename = do
  exists <- doesFileExist filename
  if not exists
    then throwIO $ FileNotExist $ "Error! No such file exists: '" ++ filename ++ "'"
    else return ()

nameify :: FilePath -> FilePath
nameify file
  | any (== '.') file = file 
  | otherwise         = file ++ ".pw"

newVerifier :: IO B.ByteString
newVerifier = getEntropy vLength


verifiedRead :: Password -> FilePath -> IO [(Tag, Password)]
verifiedRead file_pw filename = do
  contents <- strictRead filename
  let plainVerifier   = B.take vLength contents
  let encryptVerifier = B.take vLength $ B.drop vLength contents
  let corrctPassword  = decryptByteString file_pw encryptVerifier == plainVerifier
  if corrctPassword
    then return $ decryptPasswords file_pw $ B.drop (vLength + vLength) contents
    else throwIO $ BadPassword $ "Error! The password is incompatible with the file: '" ++ filename ++ "'"

    where
      decryptPasswords :: Password -> B.ByteString -> [(Tag, Password)]
      decryptPasswords _ (PS _ _ 0) = [] 
      decryptPasswords file_pw bs   = (tag, password) : decryptPasswords file_pw rest
        where
          tagLength, pwLength :: Int
          tagLength = fromEnum $ B.head bs
          pwLength  = fromEnum $ (B.head . B.tail) bs
          decrypted = decrypt file_pw $ B.take (tagLength + pwLength) $ B.drop 2 bs
          tag       = take tagLength decrypted
          password  = take pwLength $ drop tagLength decrypted
          rest      = B.drop (2 + tagLength + pwLength) bs


------------------------------------------------------
createNewFile :: FilePath -> Password -> IO ()
createNewFile file file_pw = do
  checkNotExists filename  
  write filename file_pw []
    where
      filename = nameify file


addPasswordToFile :: FilePath -> Tag -> Password -> Password -> IO ()
addPasswordToFile file tag pw file_pw = do
  checkExists filename
  contents <- verifiedRead file_pw filename
  write filename file_pw $ contents ++ [(tag,pw)]
    where
      filename  = nameify file

readPasswordsFromFile :: FilePath -> Password -> IO [(Tag, Password)]
readPasswordsFromFile file file_pw = do
  checkExists filename
  verifiedRead file_pw filename
    where
      filename = nameify file

getPasswordsFromFile :: FilePath -> Tag -> Password -> IO [String]
getPasswordsFromFile file tag file_pw = do
  checkExists filename
  contents <- verifiedRead file_pw filename
  return $ lookupAll tag contents
    where
      filename = nameify file

removePasswordFromFile :: FilePath -> Tag -> Password -> IO ()
removePasswordFromFile file tag file_pw = do
  checkExists filename
  contents  <- verifiedRead file_pw filename
  verifiers <- verifierPair file_pw
  write filename file_pw $ removeTaggedLines contents
    where
      filename = nameify file
      
      removeTaggedLines [] = []
      removeTaggedLines ((t,pw):pws)
        | t == tag  = removeTaggedLines pws
        | otherwise = (t,pw) : removeTaggedLines pws

verifierPair :: Password -> IO B.ByteString
verifierPair file_pw = do
  verifier <- newVerifier
  return $ B.append verifier (encryptByteString file_pw verifier)

write :: FilePath -> Password -> [(Tag, Password)] -> IO ()
write file file_pw pws = do
  verifiers <- verifierPair file_pw
  B.writeFile file $ B.append verifiers $ encryptPasswords pws
  where 
    encryptPasswords :: [(Tag, Password)] -> B.ByteString
    encryptPasswords [] = B.empty
    encryptPasswords ((t,p):ps) = B.append (encryptPassword t p) (encryptPasswords ps)

    encryptPassword :: Tag -> Password -> B.ByteString
    encryptPassword tag pw = B.cons tLength $ B.cons pwLength encrypted
      where
        encrypted = encrypt file_pw (tag ++ pw)
        tLength   = toEnum $ length tag
        pwLength  = toEnum $ length pw
