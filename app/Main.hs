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
        Right image ->
            let imgWidth = dynWidth image
                imgHeight = dynHeight image
                rgbImage = convertRGB8 image

                pixels =
                    map
                        ( \y ->
                            map
                                (\x -> pixelAt rgbImage x y)
                                [0 .. imgWidth - 1]
                        )
                        [0 .. imgHeight - 1]

                brightnessList = map (map brightness) pixels
                charsList = map (map toChar) brightnessList
             in writeFile "./output.txt" (unlines charsList)

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
