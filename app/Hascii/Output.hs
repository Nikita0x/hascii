module Hascii.Output where

import           System.Directory
import           System.Exit      (die)
import           System.FilePath



writeOutput :: FilePath -> String -> IO ()
writeOutput outputPath content = do
    exists <- doesFileExist outputPath

    if exists
        then die "Error: output file already exists!"
        else do
            let directory = takeDirectory outputPath

            createDirectoryIfMissing True directory

            writeFile outputPath content
