module Tree.Packman where

import Tree.Types
import Data.ByteString.Lazy as BS
import Data.Binary     as B
import GHC.Packing

import System.IO.Unsafe -- don't want to rewrite the whole thing just yet
import Control.DeepSeq
import Tree.DeepSeq

serialise :: Tree -> BS.ByteString
serialise t = rnf t `seq`
              (B.encode . unsafePerformIO . trySerialize) t
              -- unsafePerformIO $ B.encode <$> trySerialize t

deserialise :: BS.ByteString -> Tree
deserialise = unsafePerformIO . deserialize . B.decode
