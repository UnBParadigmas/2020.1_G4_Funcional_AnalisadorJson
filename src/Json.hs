module JsonParser 
    (   Parser (..)
        , Json (..)
        , jsonParser
    ) where

import Control.Applicative ( Alternative((<|>), empty, many) ) 
import Data.Char (isDigit, isSpace)

data Json = 
    JsonNull
    | JsonBool Bool
    | JsonNumber 
    | JsonString String
    | JsonArray [Json]
    | JsonObject [(String, Json)]
    deriving (Eq, Show)

newtype Parser c = Parser {
    runParser :: String -> Maybe (String, c)
} 

instance Functor Parser where
    fmap f (Parser p) = 
        Parser $ \x -> do
            (x', c) <- p x
            Just (x', f c)

instance Applicative Parser where
    pure p = Parser $ \x -> Just (x, p)
    (Parser p) <*> (Parser pr) = 
        Parser $ \x -> do
            (x', f) <- p x
            (x'', c) <- pr x'
            Just (x'', f c)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p) <|> (Parser pr) = 
        Parser $ \x -> 
            case runParser (Parser p) x of
                Just value -> Just value
                _          -> runParser (Parser pr) x

helperCharParser :: Char -> Parser Char
helperCharParser a = Parser $ \x ->
    case x of
    z:zs 
        | z == a      -> Just (zs, a)
        | otherwise   -> Nothing
    [] -> Nothing

helperStringParser :: String -> Parser String
helperStringParser [] = Parser $ \x -> Just (x, "")
helperStringParser (x:xs) = Parser $ \c -> do
   (cs, _)  <- runParser (helperCharParser x) c 
   (cs', _) <- runParser (helperStringParser xs) cs
   pure (cs', x:xs) 

-- Separa os a string em numeros e o resto ex: "123hello" => ("hello", "123")
spanHelperParser :: (Char -> Bool) -> Parser String
spanHelperParser f = 
  Parser $ \x ->
    let (token, rest) = span f x
     in pure (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \x -> do
        (x', xs) <- p x
        if null xs
            then Nothing
            else pure (x', xs)

-- funcao que utiliza um separador especifico para dividir varios valores pelo separador
-- many :: f a -> f [a], funcao que aplica a funcao para varios valores do mesmo tipo ate dar errado
-- (sep *> x) diz que cada valor x vai ser separado pelo separador 
-- para poder transformar x em [x], utiliza <*> para entrar na lista e <$> para adicionar na lista
-- e por fim, para poder usar o valor vazio, utiliza <|> com uma lista vazia
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy separator x = (:) <$> x <*> many (separator *> x) <|> pure [] 

jsonNullParser :: Parser Json
jsonNullParser = Parser $ \x -> do
    (xs, _) <- runParser (helperStringParser "null") x
    pure (xs, JsonNull)

jsonTrueParser :: Parser Json
jsonTrueParser = Parser $ \x -> do
    (xs, _) <- runParser (helperStringParser "true") x
    pure (xs, JsonBool True)

jsonFalseParser :: Parser Json
jsonFalseParser = Parser $ \x -> do
    (xs, _) <- runParser (helperStringParser "false") x
    pure (xs, JsonBool False)

jsonBoolParser :: Parser Json
jsonBoolParser = jsonFalseParser <|> jsonTrueParser

jsonNumberParser :: Parser Json
jsonNumberParser = Parser $ \x -> do 
    (xs, _) <- runParser (notNull (spanHelperParser isDigit)) x
    pure(xs, JsonNumber)

-- utiliza o operador *> que significa, retornar o valor para que esta apontado
-- applicatives possuem um estado interior que, no caso, deve ser manipulado
-- nesse caso, o que deve ser manipulado eh o estado do parser, os valores que devem ser passados a frente
-- entao isso eh util para combinar os parsers, pois podem ter varios parsers combinados com o retorno de apenas 1
simpleString :: (Char -> Bool) -> Parser String
simpleString = \x -> 
    helperCharParser '"' *> spanHelperParser x <* helperCharParser '"'

jsonStringParser :: Parser Json
jsonStringParser = JsonString <$> simpleString (/= '"')
-- precisa da operacao fmap pois se torna necessario mudar o tipo da variavel para JsonString (constructor de Json)

jsonArrayParser :: Parser Json
jsonArrayParser = JsonArray <$> (helperCharParser '[' *> spanHelperParser isSpace *> elements <* spanHelperParser isSpace <* helperCharParser ']')
    where
        elements = sepBy(spanHelperParser isSpace *> helperCharParser ',' <* spanHelperParser isSpace) jsonParser

jsonObjectParser :: Parser Json
jsonObjectParser = (helperCharParser '{' *> spanHelperParser isSpace *> values <* spanHelperParser isSpace <* helperCharParser '}')
    where
        values = 
            JsonObject <$> sepBy (spanHelperParser isSpace *> helperCharParser ',' <* spanHelperParser isSpace) value
        value = 
            (\key _ value -> (key, value)) <$> simpleString (/= '"') <*> 
            (spanHelperParser isSpace *> helperCharParser ':' *> spanHelperParser isSpace) 
            <*> jsonParser
            

jsonParser :: Parser Json
jsonParser = 
    jsonNullParser <|> jsonBoolParser <|> jsonNumberParser <|> jsonStringParser <|> jsonArrayParser <|> jsonObjectParser

