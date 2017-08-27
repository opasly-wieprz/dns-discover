module IP where

import Data.List (intercalate)
import Data.Word (Word8)

data IP = IP Word8 Word8 Word8 Word8

instance Show IP where
  show (IP a b c d) = intercalate "." $ map show [a, b, c, d]
