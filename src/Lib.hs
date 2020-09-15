module Lib
    (
        Json
    ) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Control.Applicative

data Json = 
    JsonNull
    | JsonBool Bool
    | JsonNumber 
    | JsonString String
    | JsonArray [Json]
    | JsonObj (Map.Map String Json)
    deriving (Eq, Show)

newtype Parser c = Parser {
    runParser :: String -> Maybe (c, String)
} 

helperCharParser :: Char -> Parser Char
helperCharParser a = Parser $ \x ->
    case x of
    z:zs 
        | z == a      -> Just (a, zs)
        | otherwise   -> Nothing
    [] -> Nothing

-- helperCharParser :: Char -> Parser Char
-- helperCharParser a = Parser f
--     where
--     f (z:zs) 
--         | z == a = Just (a, zs)
--         | otherwise = Nothing
--     f [] = Nothing

-- Jeito 1 de fazer: Utilizando diretamente o runParser e utilizando do operador "do" para fazer o parse do json

helperStringParser :: String -> Parser String
helperStringParser [] = Parser $ \x -> Just ("", x)
helperStringParser (x:xs) = Parser $ \c -> do
   (_, cs)  <- runParser (helperCharParser x) c 
   (_, cs') <- runParser (helperStringParser xs) cs
   pure (x:xs, cs') 

-- Separa os a string em numeros e o resto ex: "123hello" => ("hello", "123")
spanParser :: (Char -> Bool) -> Parser String
spanParser f = 
  Parser $ \x ->
    let (token, rest) = span f x
     in Just (rest, token)

jsonNullParser :: Parser Json
jsonNullParser = Parser $ \x -> do
    (_, xs) <- runParser (helperStringParser "null") x
    pure (JsonNull, xs)

jsonTrueParser :: Parser Json
jsonTrueParser = Parser $ \x -> do
    (_, xs) <- runParser (helperStringParser "true") x
    pure (JsonBool True, xs)

jsonFalseParser :: Parser Json
jsonFalseParser = Parser $ \x -> do
    (_, xs) <- runParser (helperStringParser "false") x
    pure (JsonBool False, xs)

jsonNumberParser :: Parser Json
jsonNumberParser = Parser $ \x -> do 
    (_, xs) <- runParser (spanParser isDigit) x
    pure(JsonNumber, xs)


-- Jeito 2 de fazer: Instanciando Functor + Applicative + Monad (pois estamos usando o Monad Maybe) manualmente
-- e utilizando map para fazer o parser de string e do json, alem de utilizar functor para alterar o tipo do jsonNull

-- instance Functor Parser where
--     fmap f (Parser p) = 
--         Parser $ \x -> do
--             (c, x') <- p x
--             Just (f c, x')

-- instance Applicative Parser where
--     pure p = Parser $ \x -> Just (p, x)
--     (Parser p) <*> (Parser pr) = 
--         Parser $ \x -> do
--             (f, x') <- p x
--             (c, x'') <- pr x'
--             Just (f c, x'')

-- helperStringParser :: String -> Parser String
-- helperStringParser = sequenceA . map helperCharParser 

-- jsonNullParser :: Parser Json
-- jsonNullParser = (\_ -> JsonNull) <$> helperStringParser "null"