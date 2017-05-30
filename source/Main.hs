{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word8)
import System.Clock
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Printf

import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Foreign.Storable         as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

createByteStringWithPoke :: Int -> Word8 -> ByteString
createByteStringWithPoke n c = B.unsafeCreate n fill where
    fill p = f 0 where f !i | i >= n = pure ()
                            | i <  n = do S.pokeElemOff p i c
                                          f (i + 1)

createByteStringWithUnfoldr :: Int -> Word8 -> ByteString
createByteStringWithUnfoldr n c = B.unfoldr f 0
    where f i | i >= n = Nothing
              | i <  n = Just (c, i + 1)

createByteStringWithUnfoldrN :: Int -> Word8 -> ByteString
createByteStringWithUnfoldrN n c = fst $ B.unfoldrN n (const $ Just (c, c)) c

measureDurationMicros :: (a -> b) -> a -> IO (b, Integer)
measureDurationMicros f a = do
    start <- getTime Monotonic
    let !b = f a
    end <- getTime Monotonic
    pure (b, toNanoSecs (end - start) `div` 1000)

prettyInteger :: (Num a, Ord a, Show a) => a -> Text
prettyInteger a =
    if a < 0
    then "-" <> prettyInteger (-a)
    else T.reverse
       $ T.intercalate ","
       $ T.chunksOf 3
       $ T.reverse
       $ T.pack
       $ show a

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering

        let char = 0x24
        let sizesMiB = [2, 4 .. 64]
        let printPadded n = T.putStr . T.justifyRight n ' '
        let replicate' = createByteStringWithUnfoldrN

        printPadded 20     "n:length (MiB)"
        printPadded 20  "a:replicate  (μs)"
        printPadded 20  "b:replicate' (μs)"
        printPadded  8                "b/a"
        putStrLn ""

        forM_ sizesMiB $ \sizeMiB -> do
            let sizeBytes = sizeMiB * 1024 * 1024
            (_, a) <- measureDurationMicros (B.replicate  sizeBytes) char
            (_, b) <- measureDurationMicros (  replicate' sizeBytes) char
            printPadded 20 $ prettyInteger sizeMiB
            printPadded 20 $ prettyInteger a
            printPadded 20 $ prettyInteger b
            printPadded  8 $ prettyInteger (b `div` a)
            putStrLn ""

