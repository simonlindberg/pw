import qualified Tests.TestUtil as A
import qualified Tests.TestCipher as B
import qualified Tests.TestGenerate as C
import qualified Tests.TestFileHandling as D

import System.Directory

test :: IO ()
test = A.test >> B.test >> C.test >> D.test

-- *** Exception: DeleteFile "file.txt": permission denied (The process cannot access the file because it is being used by another process.)

main :: IO ()
main = test