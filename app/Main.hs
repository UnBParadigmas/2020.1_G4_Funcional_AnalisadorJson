module Main where

import JsonParser

run :: String -> Maybe (String, Json)
run x = runParser jsonParser x

-- funcao 
parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile file parser = do
    input <- readFile file
    return (snd <$> runParser parser input) 

main :: IO ()
main = undefined

















