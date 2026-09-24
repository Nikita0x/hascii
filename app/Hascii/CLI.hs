{-# OPTIONS_GHC -fmax-pmcheck-models=1000 #-}

module Hascii.CLI where

import           System.Environment (getArgs)
import           System.Exit        (die, exitSuccess)
import           Text.Read          (readMaybe)


data Config = Config
    {
      imagePath   :: FilePath,
      outputPath  :: FilePath,
      targetWidth :: Maybe Int
    } deriving (Show)


parseArgs :: IO Config
parseArgs = do

    args <- getArgs

    case args of
        [image , "--width" , amount , "--output" , outPath] ->
            case readMaybe amount of
                Just width
                    | width > 0 ->
                        return Config
                            { imagePath = image
                            , targetWidth = Just width
                            , outputPath = outPath
                            }

                    | otherwise ->
                        die "Error: width must be greater than 0!"

                Nothing ->
                    die "Error: width must be an integer!"

        [image, "--width", amount] ->
            case readMaybe amount of
                Just width
                    | width > 0 ->
                        return Config
                            { imagePath = image
                            , targetWidth = Just width
                            , outputPath = "./output.txt"
                            }

                    | otherwise ->
                        die "Error: width must be greater than 0!"

                Nothing ->
                    die "Error: width must be an integer!"

        ["--help"] ->
            putStrLn helpMessage >> exitSuccess

        ["-h"] ->
            putStrLn helpMessage >> exitSuccess

        [firstArg] ->
            return Config
            { imagePath = firstArg
            , targetWidth = Nothing
            , outputPath = "./output.txt"
            }

        [] ->
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
