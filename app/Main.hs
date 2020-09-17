module Main where

import JsonParser

run :: String -> Maybe (String, Json)
run x = runParser jsonParser x

-- function to get only the second element of a tuple
secondElementParser :: (a, b) -> b
secondElementParser (x, y) = y

readAndParseFile :: FilePath -> Parser a -> IO (Maybe a)
readAndParseFile x p = do
    file <- readFile x
    parseFile file p

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile file p = do
    return (secondElementParser <$> runParser p file) 

main :: IO ()
main = undefined

















