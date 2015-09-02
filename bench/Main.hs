{-# OPTIONS_GHC -fno-cse -fno-ignore-asserts #-}
module Main where

import qualified Real.Types     as Types
import qualified Real.MemSize
import           Real.DeepSeq ()
import qualified Real.Load as Load

import qualified Real.ReadShow  as ReadShow
import qualified Real.PkgBinary as PkgBinary
import qualified Real.PkgCereal as PkgCereal
import qualified Real.PkgAesonGeneric as PkgAesonGeneric
import qualified Real.PkgAesonTH as PkgAesonTH
--import qualified Real.PkgMsgpack as PkgMsgpack
import qualified Real.CBOR as CBOR
import qualified Real.Packman as Packman

import qualified Tree.MemSize
import           Tree.DeepSeq ()
import qualified Tree.Load      as Micro.Load
import qualified Tree.Types     as Micro.Types ()
import qualified Tree.ReadShow  as Micro.ReadShow
import qualified Tree.PkgBinary as Micro.PkgBinary
import qualified Tree.PkgCereal as Micro.PkgCereal
import qualified Tree.PkgAesonGeneric as Micro.PkgAesonGeneric
import qualified Tree.PkgAesonTH as Micro.PkgAesonTH
--import qualified Real.PkgMsgpack as PkgMsgpack
import qualified Tree.CBOR as Micro.CBOR
import qualified Tree.Packman as Micro.Packman

import Criterion.Main

import Data.Int
import qualified Data.ByteString.Lazy   as BS
import qualified Codec.Compression.GZip as GZip
import Control.Exception
import Control.DeepSeq
import System.Environment
import Data.Time


main :: IO ()
main = do
    args <- getArgs
    case args of
      ("bench":args') -> withArgs args' benchmarks

      ("microbench":args') -> withArgs args' microbenchmarks

      ["prep-decode-binary"] -> do
        Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                            (BS.readFile "bench/00-index.tar.gz")
        let pkgs = take 20000 pkgs_
        BS.writeFile "bench/binary.bin" (PkgBinary.serialise pkgs)

      ["prep-decode-cereal"] -> do
        Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                            (BS.readFile "bench/00-index.tar.gz")
        let pkgs = take 20000 pkgs_
        BS.writeFile "bench/cereal.bin" (PkgCereal.serialise pkgs)

      ["prep-decode-cbor"] -> do
        Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                            (BS.readFile "bench/00-index.tar.gz")
        let pkgs = take 20000 pkgs_
        BS.writeFile "bench/cbor.bin" (CBOR.serialise pkgs)

      ["prep-decode-cbor-small"] -> do
        Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                            (BS.readFile "bench/00-index.tar.gz")
        let pkgs = take 1000 pkgs_
        BS.writeFile "bench/cbor-small.bin" (CBOR.serialise pkgs)

      ["perf-decode-binary"] -> do
        pkgdata <- BS.readFile "bench/binary.bin"
        time $ evaluate (BS.length pkgdata)
        time $ evaluate (length $ PkgBinary.deserialise pkgdata)

      ["perf-decode-cereal"] -> do
        pkgdata <- BS.readFile "bench/cereal.bin"
        time $ evaluate (BS.length pkgdata)
        time $ evaluate (length $ PkgCereal.deserialise pkgdata)

      ["perf-decode-cbor"] -> do
        pkgdata <- BS.readFile "bench/cbor.bin"
        time $ evaluate (BS.length pkgdata)
        time $ evaluate (length $ CBOR.deserialise pkgdata)

      ["perf-decode-binary-noaccum"] -> do
        pkgdata <- BS.readFile "bench/binary.bin"
        time $ evaluate (BS.length pkgdata)
        time $ evaluate (PkgBinary.deserialiseNull pkgdata)

      ["perf-decode-cereal-noaccum"] -> do
        pkgdata <- BS.readFile "bench/cereal.bin"
        time $ evaluate (BS.length pkgdata)
        time $ evaluate (PkgCereal.deserialiseNull pkgdata)

      ["perf-decode-cbor-noaccum"] -> do
        pkgdata <- BS.readFile "bench/cbor.bin"
        time $ evaluate (BS.length pkgdata)
        time $ evaluate (CBOR.deserialiseNull pkgdata)

      ["perf-decode-cbor-noaccum-small"] -> do
        pkgdata <- BS.readFile "bench/cbor-small.bin"
        time $ evaluate (BS.length pkgdata)
        time $ evaluate (CBOR.deserialiseNull pkgdata)


benchmarks :: IO ()
benchmarks =
    defaultMain macrobenchmarks

readBigTestData :: IO [Types.GenericPackageDescription]
readBigTestData = do
    Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                        (BS.readFile "bench/00-index.tar.gz")
    let tstdata  = take 100 pkgs_
    return tstdata

macrobenchmarks :: [Benchmark]
macrobenchmarks =
  [ env readBigTestData $ \tstdata ->
    bgroup "reference"
      [ bench "deepseq" (whnf rnf tstdata)
      , bench "memSize" (whnf (flip Real.MemSize.memSize 0) tstdata)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "encoding"
      [ bench "binary"        (whnf perfEncodeBinary       tstdata)
--      , bench "cereal"        (whnf perfEncodeCereal       tstdata)
--      , bench "aeson generic" (whnf perfEncodeAesonGeneric tstdata)
--      , bench "aeson TH"      (whnf perfEncodeAesonTH      tstdata)
      , bench "read/show"     (whnf perfEncodeReadShow     tstdata)
--      , bench "msgpack lib"   (whnf perfEncodeMsgpack      tstdata)
--      , bench "new msgpack"   (whnf perfEncodeNewMsgPack   tstdata)
--      , bench "cbor"          (whnf perfEncodeCBOR         tstdata)
      , bench "packman"       (whnf perfEncodePackman      tstdata)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "decoding whnf"
      [ env (return $ PkgBinary.serialise tstdata) $ \tstdataB ->
        bench "binary"        (whnf perfDecodeBinary       tstdataB)

      -- , env (return $ PkgCereal.serialise tstdata) $ \tstdataC ->
      --   bench "cereal"        (whnf perfDecodeCereal       tstdataC)

      -- , env (return $ PkgAesonTH.serialise tstdata) $ \tstdataA ->
      --   bgroup "aeson"
      --     [ bench "generic"   (whnf perfDecodeAesonGeneric tstdataA)
      --     , bench "TH"        (whnf perfDecodeAesonTH      tstdataA)
      --     ]

      , env (return $ ReadShow.serialise tstdata) $ \tstdataS ->
        bench "read/show"     (whnf perfDecodeReadShow     tstdataS)

--      , bench "msgpack lib"   (whnf perfDecodeMsgpack      tstdataM)

--      , env (return $ NewMsgpack.serialise tstdata) $ \tstdataN ->
--        bench "new msgpack"   (whnf perfDecodeNewMsgPack   tstdataN)

      -- , env (return $ CBOR.serialise tstdata) $ \tstdataR ->
      --   bench "cbor"   (whnf perfDecodeCBOR                tstdataR)

      , env (return $ Packman.serialise tstdata) $ \tstdataR ->
        bench "packman"   (whnf perfDecodePackman                tstdataR)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "decoding nf"
      [ env (return $ PkgBinary.serialise tstdata) $ \tstdataB ->
        bench "binary"        (nf perfDecodeBinary       tstdataB)

      -- , env (return $ PkgCereal.serialise tstdata) $ \tstdataC ->
      --   bench "cereal"        (nf perfDecodeCereal       tstdataC)

      -- , env (return $ PkgAesonTH.serialise tstdata) $ \tstdataA ->
      --   bgroup "aeson"
      --     [ bench "generic"   (nf perfDecodeAesonGeneric tstdataA)
      --     , bench "TH"        (nf perfDecodeAesonTH      tstdataA)
      --     ]

      , env (return $ ReadShow.serialise tstdata) $ \tstdataS ->
        bench "read/show"     (nf perfDecodeReadShow     tstdataS)

--      , bench "msgpack lib"   (nf perfDecodeMsgpack      tstdataM)

--      , env (return $ NewMsgpack.serialise tstdata) $ \tstdataN ->
--        bench "new msgpack"   (nf perfDecodeNewMsgPack   tstdataN)

      -- , env (return $ CBOR.serialise tstdata) $ \tstdataR ->
      --   bench "cbor"          (nf perfDecodeCBOR         tstdataR)

      , env (return $ Packman.serialise tstdata) $ \tstdataR ->
        bench "packman"          (nf perfDecodePackman         tstdataR)
      ]
  ]
  where
    perfEncodeBinary, perfEncodeCereal, perfEncodeAesonGeneric,
      perfEncodeAesonTH, perfEncodeReadShow,
      perfEncodeCBOR, perfEncodePackman
      :: [Types.GenericPackageDescription] -> Int64


    perfEncodeBinary       = BS.length . PkgBinary.serialise
    perfEncodeCereal       = BS.length . PkgCereal.serialise
    perfEncodeAesonGeneric = BS.length . PkgAesonGeneric.serialise
    perfEncodeAesonTH      = BS.length . PkgAesonTH.serialise
    perfEncodeReadShow     = BS.length . ReadShow.serialise
    --perfEncodeMsgpack      = BS.length . PkgMsgpack.serialise
    --perfEncodeNewMsgPack   = BS.length . NewMsgpack.serialise
    perfEncodeCBOR         = BS.length . CBOR.serialise
    perfEncodePackman      = BS.length . Packman.serialise

    perfDecodeBinary, perfDecodeCereal, perfDecodeAesonGeneric,
      perfDecodeAesonTH, perfDecodeReadShow,
      perfDecodeCBOR, perfDecodePackman
      :: BS.ByteString -> [Types.GenericPackageDescription]


    perfDecodeBinary       = PkgBinary.deserialise
    perfDecodeCereal       = PkgCereal.deserialise
    perfDecodeAesonGeneric = PkgAesonGeneric.deserialise
    perfDecodeAesonTH      = PkgAesonTH.deserialise
    perfDecodeReadShow     = ReadShow.deserialise
    --perfDecodeMsgpack      = PkgMsgpack.deserialise
    --perfDecodeNewMsgPack   = NewMsgpack.deserialise
    perfDecodeCBOR        = CBOR.deserialise
    perfDecodePackman     = Packman.deserialise

microbenchmarks :: IO ()
microbenchmarks = do
    let tstdata = Micro.Load.mkBigTree 16 -- tree of size 2^16
        
        tstdataB = Micro.PkgBinary.serialise tstdata
        tstdataC = Micro.PkgCereal.serialise tstdata
        tstdataA = Micro.PkgAesonTH.serialise tstdata
        tstdataS = Micro.ReadShow.serialise tstdata
--        tstdataM = PkgMsgpack.serialise tstdata
--        tstdataN = Micro.NewMsgpack.serialise tstdata
        tstdataR = Micro.CBOR.serialise tstdata
        tstdataP = Micro.Packman.serialise tstdata
    defaultMain
      [ bgroup "reference"
          [ bench "deepseq" (whnf rnf tstdata)
          , bench "memSize" (whnf (flip Tree.MemSize.memSize 0) tstdata)
          ]
      , bgroup "encoding" $ deepseq tstdata
          [ bench "binary"        (whnf perfEncodeBinary       tstdata)
          -- , bench "cereal"        (whnf perfEncodeCereal       tstdata)
          -- , bench "aeson generic" (whnf perfEncodeAesonGeneric tstdata)
          -- , bench "aeson TH"      (whnf perfEncodeAesonTH      tstdata)
          , bench "read/show"     (whnf perfEncodeReadShow     tstdata)
    --      , bench "msgpack lib"   (whnf perfEncodeMsgpack      tstdata)
    --      , bench "new msgpack"   (whnf perfEncodeNewMsgPack   tstdata)
          -- , bench "cbor"          (whnf perfEncodeCBOR         tstdata)
          , bench "packman"       (whnf perfEncodePackman      tstdata)
          ]
      , bgroup "decoding" $ deepseq (tstdataB, tstdataC, tstdataA, tstdataS,
                                      tstdataR)
          [ bench "binary"        (whnf perfDecodeBinary       tstdataB)
          -- , bench "cereal"        (whnf perfDecodeCereal       tstdataC)
          -- , bench "aeson generic" (whnf perfDecodeAesonGeneric tstdataA)
          -- , bench "aeson TH"      (whnf perfDecodeAesonTH      tstdataA)
          , bench "read/show"     (whnf perfDecodeReadShow     tstdataS)
    --      , bench "msgpack lib"   (whnf perfDecodeMsgpack      tstdataM)
    --      , bench "new msgpack"   (whnf perfDecodeNewMsgPack   tstdataN)
          -- , bench "cbor"          (whnf perfDecodeCBOR         tstdataR)
          , bench "packman"       (whnf perfDecodePackman      tstdataP)
          ]
      , bgroup "decoding + deepseq" $ deepseq (tstdataB, tstdataC, tstdataA, 
                                               tstdataS, tstdataR, tstdataP)
          [ bench "binary"        (nf perfDecodeBinary       tstdataB)
          -- , bench "cereal"        (nf perfDecodeCereal       tstdataC)
          -- , bench "aeson generic" (nf perfDecodeAesonGeneric tstdataA)
          -- , bench "aeson TH"      (nf perfDecodeAesonTH      tstdataA)
          , bench "read/show"     (nf perfDecodeReadShow     tstdataS)
    --      , bench "msgpack lib"   (nf perfDecodeMsgpack      tstdataM)
    --      , bench "new msgpack"   (nf perfDecodeNewMsgPack   tstdataN)
          -- , bench "cbor"          (nf perfDecodeCBOR         tstdataR)
          , bench "packman"       (nf perfDecodePackman      tstdataP)
          ]
      ]
  where

    perfEncodeBinary       = BS.length . Micro.PkgBinary.serialise
    perfEncodeCereal       = BS.length . Micro.PkgCereal.serialise
    perfEncodeAesonGeneric = BS.length . Micro.PkgAesonGeneric.serialise
    perfEncodeAesonTH      = BS.length . Micro.PkgAesonTH.serialise
    perfEncodeReadShow     = BS.length . Micro.ReadShow.serialise
    --perfEncodeMsgpack      = BS.length . Micro.PkgMsgpack.serialise
    --perfEncodeNewMsgPack   = BS.length . Micro.NewMsgpack.serialise
    perfEncodeCBOR         = BS.length . Micro.CBOR.serialise
    perfEncodePackman      = BS.length . Micro.Packman.serialise

    perfDecodeBinary       = Micro.PkgBinary.deserialise
    perfDecodeCereal       = Micro.PkgCereal.deserialise
    perfDecodeAesonGeneric = Micro.PkgAesonGeneric.deserialise
    perfDecodeAesonTH      = Micro.PkgAesonTH.deserialise
    perfDecodeReadShow     = Micro.ReadShow.deserialise
    --perfDecodeMsgpack      = PkgMsgpack.deserialise
    --perfDecodeNewMsgPack   = Micro.NewMsgpack.deserialise
    perfDecodeCBOR         = Micro.CBOR.deserialise
    perfDecodePackman      = Micro.Packman.deserialise

time :: Show a => IO a -> IO ()
time action = do
    t   <- getCurrentTime
    x   <- action
    t'  <- getCurrentTime
    putStrLn $ show (diffUTCTime t' t) ++ ":\t" ++ show x

