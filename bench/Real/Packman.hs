module Real.Packman where

import Real.Types

import Data.ByteString.Lazy as BS
import Data.Binary     as B
import GHC.Packing

import System.IO.Unsafe -- don't want to rewrite the whole thing just yet

import Control.DeepSeq
import Real.DeepSeq

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise t = rnf t `seq`
              (B.encode . unsafePerformIO . trySerialize) t

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = unsafePerformIO . deserialize . B.decode
