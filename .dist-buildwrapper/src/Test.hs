module Main where

import Control.Applicative
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BSL
import Data.Word

serialize :: [Integer] -> Put
serialize [] = return ()
serialize (x:xs) = do
        putWord32be (fromIntegral x :: Word32)
        serialize xs

deserialize :: Get [Integer]
deserialize = do
        empty <- isEmpty
        if empty then return [] else
                do
                x <- getWord32be
                xs <- deserialize
                return (fromIntegral x:xs)

main = do
        list <- runGet deserialize <$> BSL.readFile "in.txt"
        let res = filter (`elem` [1..10]) list
        BSL.writeFile "out.txt" $ runPut $ serialize res