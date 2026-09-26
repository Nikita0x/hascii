{-# LANGUAGE OverloadedRecordDot #-}

module Hascii.Convert where

import           Codec.Picture
import           Hascii.ASCII

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img

pixelInfoToColoredGlyph :: PixelInfo -> ColoredGlyph
pixelInfoToColoredGlyph input =
    let char = toChar (brightnessValue input)
        form = glyph char
    in map (map (\x -> (x, pixel input))) form


{-|
Принимает два глифа и объединяет их построчно.

glyph '*' =
    [ "   ##   "
    , "## ## ##"
    , " #####  "
    , "  ##### "
    , " #####  "
    , "## ## ##"
    , "   ##   "
    , "        "
    ]

glyph '+' =
    [ "   ##   "
    , "   ##   "
    , "   ##   "
    , "########"
    , "########"
    , "   ##   "
    , "   ##   "
    , "   ##   "
    ]

Результат:

"   ##      ##   "
"## ## ##   ##   "
" #####     ##   "
"  ##### ########"
" #####  ########"
"## ## ##   ##   "
"   ##      ##   "
"           ##   "
-}
combineGlyphs :: Glyph -> Glyph -> Glyph
combineGlyphs glyph1 glyph2 =
    -- zipWith - возвращает ОДИН список
    -- Принимает 3 аргумента - функцию и 2 списка
    -- Возьми первый элемент из glyph1 и первый элемент из glyph2, примени к ним (++). Потом второй и второй. Потом третий и третий...
    -- zipWith (+) [1, 2, 3] [10, 20, 30]
    zipWith (++) glyph1 glyph2

combineColoredGlyphs :: ColoredGlyph -> ColoredGlyph -> ColoredGlyph
combineColoredGlyphs = zipWith (++)

combineGlyphRow :: [Glyph] -> Glyph
combineGlyphRow (firstGlyph : restGlyphs) =
    foldl combineGlyphs firstGlyph restGlyphs
combineGlyphRow [] = []

combineColoredGlyphRow :: [ColoredGlyph] -> ColoredGlyph
combineColoredGlyphRow (firstGlyph : restGlyphs) =
    foldl combineColoredGlyphs firstGlyph restGlyphs
combineColoredGlyphRow [] = []

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

coloredGlyphToImage :: ColoredGlyph -> Image PixelRGB8
coloredGlyphToImage glyph =
    generateImage pixelAt' width height
  where
    height = length glyph
    width  = length (head glyph)

    pixelAt' x y =
        let (char, color) = (glyph !! y) !! x
        in if char == ' '
            then PixelRGB8 255 255 255
            else color

writeColoredGlyphPng :: FilePath -> ColoredGlyph -> IO ()
writeColoredGlyphPng path glyph =
    writePng path (coloredGlyphToImage glyph)

{-|
Функция отвечает за цвет глифа.
Принимает Символ и возвращает Цвет RGB (Pixel)
Если пробел - то белый цвет
Все остальное - черный
-}
glyphToPixel :: Char -> PixelRGB8
glyphToPixel ' ' = PixelRGB8 255 255 255
glyphToPixel _   = PixelRGB8 0 0 0

imageToGlyph :: DynamicImage -> Maybe Int -> Either String ColoredGlyph
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
                    charsList = map (map (\x -> toChar x.brightnessValue )) brightnessList

                    glyphs = map (map pixelInfoToColoredGlyph) brightnessList
                    --         map снимает один уровень списка, было [[Glyph]], стало [Glyph]
                    combinedGlyphs = map combineColoredGlyphRow glyphs

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
                    charsList = map (map (\x -> toChar x.brightnessValue )) brightnessList

                in Right (unlines charsList)

imageToPng :: DynamicImage -> Maybe Int -> FilePath -> IO ()
imageToPng image maybeWidth pngPath =
    case imageToGlyph image maybeWidth of
        Left err ->
            putStrLn err
        Right finalGlyph ->
            writeColoredGlyphPng pngPath finalGlyph
