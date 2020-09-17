module Main where

import JsonParser

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile file parser = do
    input <- readFile file
    return (snd <$> runParser parser input) 

main :: IO ()
main = undefined

















