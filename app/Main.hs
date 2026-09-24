{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import           Codec.Picture
import           Hascii.CLI
import           Hascii.Convert
import           Hascii.Output
import           System.Exit    (die)

main :: IO ()
main = do

    config <- parseArgs

    result <- readImage config.imagePath

    case result of
        Left err ->
            putStrLn err

        Right image -> do
            case imageToAscii image (targetWidth config) of
                Left err ->
                    die err

                Right ascii ->
                    writeOutput config.outputPath ascii
