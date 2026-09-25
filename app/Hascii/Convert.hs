module Hascii.Convert where

import           Codec.Picture
import           Hascii.ASCII

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img

combineGlyphs :: Glyph -> Glyph -> Glyph
combineGlyphs glyph1 glyph2 =
    zipWith (++) glyph1 glyph2

combineGlyphRow :: [Glyph] -> Glyph
combineGlyphRow (firstGlyph : restGlyphs) =
    foldl combineGlyphs firstGlyph restGlyphs
combineGlyphRow [] = []

glyphToImage :: Glyph -> Image PixelRGB8
glyphToImage glyph =
    generateImage pixelAt' width height
  where
    height = length glyph
    width  = length (head glyph)

    pixelAt' x y =
        glyphToPixel ((glyph !! y) !! x)

writeGlyphPng :: FilePath -> Glyph -> IO ()
writeGlyphPng path glyph =
    writePng path (glyphToImage glyph)

glyphToPixel :: Char -> PixelRGB8
glyphToPixel ' ' = PixelRGB8 255 255 255
glyphToPixel _   = PixelRGB8 0 0 0

imageToGlyph :: DynamicImage -> Maybe Int -> Either String Glyph
imageToGlyph image maybeWidth =
    let imgWidth = dynWidth image
        imgHeight = dynHeight image

        width = case maybeWidth of
            Just w  -> w
            Nothing -> imgWidth

    in
        if width > imgWidth
            then Left "Error: target width cannot be greater than the original image width!"
            else
                let rgbImage = convertRGB8 image
                    stepX = imgWidth `div` width

                    targetHeight = case maybeWidth of
                        Just _  -> (imgHeight `div` stepX) `div` 2
                        Nothing -> imgHeight

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

                    glyphs = map (map glyph) charsList
                    combinedGlyphs = map combineGlyphRow glyphs

                in Right (concat combinedGlyphs)

imageToAscii :: DynamicImage -> Maybe Int -> Either String String
imageToAscii image maybeWidth =
    let imgWidth = dynWidth image
        imgHeight = dynHeight image

        width = case maybeWidth of
            Just w  -> w
            Nothing -> imgWidth

    in
        if width > imgWidth
            then Left "Error: target width cannot be greater than the original image width!"
            else
                let rgbImage = convertRGB8 image
                    stepX = imgWidth `div` width

                    targetHeight = case maybeWidth of
                        Just _  -> (imgHeight `div` stepX) `div` 2
                        Nothing -> imgHeight

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

imageToPng :: DynamicImage -> Maybe Int -> FilePath -> IO ()
imageToPng image maybeWidth pngPath =
    case imageToGlyph image maybeWidth of
        Left err ->
            putStrLn err
        Right finalGlyph ->
            writeGlyphPng pngPath finalGlyph
