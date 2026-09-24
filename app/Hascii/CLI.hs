module Hascii.CLI where

import           System.Environment (getArgs)
import           System.Exit        (die, exitSuccess)
import           Text.Read          (readMaybe)


data Config = Config
    {
      imagePath   :: FilePath,
      outputPath  :: FilePath,
      targetWidth :: Int
    } deriving (Show)


parseArgs :: IO Config
parseArgs = do

    args <- getArgs

    case args of
        (firstArg : "--width" : amount : "--output" : outPath : _) ->
            case readMaybe amount of
                Just width
                    | width > 0 ->
                        return Config
                            { imagePath = firstArg
                            , targetWidth = width
                            , outputPath = outPath
                            }

                    | otherwise ->
                        die "Error: width must be greater than 0!"

                Nothing ->
                    die "Error: width must be an integer!"

        (firstArg : "--width" : amount : _) ->
            case readMaybe amount of
                Just width
                    | width > 0 ->
                        return Config
                            { imagePath = firstArg
                            , targetWidth = width
                            , outputPath = "./output.txt"
                            }

                    | otherwise ->
                        die "Error: width must be greater than 0!"

                Nothing ->
                    die "Error: width must be an integer!"

        [] ->
            putStrLn helpMessage >> exitSuccess

        ["--help"] ->
            putStrLn helpMessage >> exitSuccess

        ["-h"] ->
            putStrLn helpMessage >> exitSuccess

        _ ->
            die "Error: invalid arguments!"




helpMessage :: String
helpMessage =
    unlines
        [ "Hascii - image to ASCII converter"
        , ""
        , "Usage:"
        , "  hascii IMAGE --width WIDTH"
        , ""
        , "Options:"
        , "  --width WIDTH       Output width"
        , "  --output PATH       Output file"
        , "  --help, -h          Show this help"
        ]
