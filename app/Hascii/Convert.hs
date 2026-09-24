module Hascii.Convert where

import           Codec.Picture
import           Hascii.ASCII

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img

imageToAscii :: DynamicImage -> Int -> Either String String
imageToAscii image width =
    let imgWidth = dynWidth image
        imgHeight = dynHeight image
    in
        if width > imgWidth
            then Left "Error: target width cannot be greater than the original image width!"
            else
                let rgbImage = convertRGB8 image
                    stepX = imgWidth `div` width
                    targetHeight = (imgHeight `div` stepX) `div` 2
                    stepY = imgHeight `div` targetHeight

                    xs = map
                        (\x -> x * stepX)
                        [0 .. width - 1]

                    ys = map
                        (\y -> y * stepY)
                        [0 .. targetHeight - 1]

                    pixels = map
                        (\y -> map (\x -> pixelAt rgbImage x y) xs)
                        ys

                    brightnessList = map (map brightness) pixels
                    charsList = map (map toChar) brightnessList

                in Right (unlines charsList)
