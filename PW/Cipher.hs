module PW.Cipher where 

import Codec.Crypto.AES
import Data.Char (ord)
import qualified System.Random as R
import qualified Data.ByteString.Char8 as B

-- In OFB mode, encryption and decryption is the same.
encryptByteString :: String -> B.ByteString -> B.ByteString
encryptByteString pw plain = crypt' OFB key iv Encrypt plain
  where
    key = B.pack $ take 32 $ drop 16 randoms
    iv  = B.pack $ take 16 randoms

    randoms :: [Char]
    randoms = R.randoms $ R.mkStdGen $ createSeed pw

    createSeed :: String -> Int
    createSeed = toInt 0
      where   
        toInt _ []     = 0
        toInt i (c:cs) = ((ord c + 1) * (base ^ i)) + toInt (i + 1) cs
        base           = 256

decryptByteString :: String -> B.ByteString -> B.ByteString
decryptByteString = encryptByteString

encrypt :: String -> String -> B.ByteString
encrypt pw plain = encryptByteString pw $ B.pack plain

decrypt :: String -> B.ByteString -> String
decrypt pw cipher = B.unpack $ decryptByteString pw cipher

