module Main (main) where

import           Codec.Picture
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Text.Read          (readMaybe)

data Config = Config
    {
      imagePath   :: FilePath,
      targetWidth :: Int
    } deriving (Show)

main :: IO ()
main = do
    args <- getArgs

    -- check for the correctness of passed arugments

    (path, targetWidth) <- case args of
        (firstArg : "--width" : amount : _) ->
            case readMaybe amount of
                Just width
                    | width > 0 -> return (firstArg, width)
                    | otherwise -> die "Error: width must be greater than 0!"
                Nothing ->
                    die "Error: width must be an integer!"
        [] ->
            die "Error: specify the image path!"
        _ ->
            die "Error: invalid arguments"

    let cfg =
            Config
                { imagePath = path
                , targetWidth = targetWidth
                }

    result <- readImage path

    -- putStrLn ("Provided image path: " ++ imagePath cfg)

    putStrLn ("Target width: " ++ show targetWidth)

    case result of
        Left err ->
            putStrLn err

        Right image -> do
            let imgWidth = dynWidth image
                imgHeight = dynHeight image

            if targetWidth > imgWidth
                then die "Error: target width cannot be greater than thhe original image width!"
                else do
                    let rgbImage = convertRGB8 image
                        stepX = imgWidth `div` targetWidth
                        targetHeight = (imgHeight `div` stepX) `div` 2 --28
                        stepY = imgHeight `div` targetHeight
                        xs = map
                            (\x -> x * stepX)
                            [0 .. targetWidth - 1]
                        ys = map (\y -> y * stepY) [0 .. targetHeight - 1]

                        pixels = map (\y -> map (\x -> pixelAt rgbImage x y) xs)
                                ys

                        brightnessList = map (map brightness) pixels
                        charsList = map (map toChar) brightnessList

                    writeFile "./output.txt" (unlines charsList)

dynWidth :: DynamicImage -> Int
dynWidth img = dynamicMap imageWidth img

dynHeight :: DynamicImage -> Int
dynHeight img = dynamicMap imageHeight img

brightness :: PixelRGB8 -> Int
brightness (PixelRGB8 r g b) =
    round
        ( 0.299 * fromIntegral r
            + 0.587 * fromIntegral g
            + 0.114 * fromIntegral b
        )

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
