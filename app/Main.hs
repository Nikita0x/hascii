{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import           Codec.Picture
import           Hascii.ASCII
import           Hascii.CLI
import           System.Directory
import           System.Exit      (die)
import           System.FilePath

main :: IO ()
main = do

    config <- parseArgs

    result <- readImage config.imagePath

    case result of
        Left err ->
            putStrLn err

        Right image -> do
            let imgWidth = dynWidth image
                imgHeight = dynHeight image
                width = targetWidth config

            if width > imgWidth
                then die "Error: target width cannot be greater than thhe original image width!"
                else do
                    let rgbImage = convertRGB8 image
                        stepX = imgWidth `div` width
                        targetHeight = (imgHeight `div` stepX) `div` 2
                        stepY = imgHeight `div` targetHeight
                        xs = map
                            (\x -> x * stepX)
                            [0 .. width - 1]
                        ys = map (\y -> y * stepY) [0 .. targetHeight - 1]

                        pixels = map (\y -> map (\x -> pixelAt rgbImage x y) xs)
                                ys

                        brightnessList = map (map brightness) pixels
                        charsList = map (map toChar) brightnessList

                    writeFile config.outputPath (unlines charsList)

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img


