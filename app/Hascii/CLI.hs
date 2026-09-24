module Hascii.CLI where

import           System.Environment (getArgs)
import           System.Exit        (die)
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
            die "Error: specify the image path!"

        _ ->
            die "Error: invalid arguments"
