module Main (main) where

import Codec.Picture

main :: IO ()
main = do
  result <- readImage "./app/something2.jpg"

  case result of
    Left err ->
      putStrLn err
    Right image ->
      let width = dynWidth image
          height = dynHeight image
          rgbImage = convertRGB8 image
          pixels =
            map
              ( \y ->
                  map
                    (\x -> pixelAt rgbImage x y)
                    [0 .. width - 1]
              )
              [0 .. height - 1]

          brightnessList = map (map brightness) pixels
          charsList = map (map toChar) brightnessList 

    --    in putStrLn (unlines charsList)
       in writeFile "./output.txt" (unlines charsList)

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img

brightness :: PixelRGB8 -> Int
brightness (PixelRGB8 r g b) = round (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)

toChar :: Int -> Char
toChar brightness
  | brightness < 30 = '@'
  | brightness < 60 = '#'
  | brightness < 90 = '*'
  | brightness < 120 = '+'
  | brightness < 150 = '-'
  | brightness < 180 = ':'
  | brightness < 210 = '.'
  | otherwise = ' '
