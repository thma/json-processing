module SimpleJson where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.List (intercalate)    

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

-- parser
spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser JValue
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ JString x

parseBool :: Parser JValue
parseBool = JBool True <$ string "true"
        <|> JBool False <$ string "false"

parseNull :: Parser JValue
parseNull = JNull <$ string "null"
         
parseNumber :: Parser JValue
-- parseNumber = liftM (JNumber . read) $ many1 (oneOf "0123456789.") --many1 digit <* char '.' <* many digit

parseNumber = do
    int <- many1 digit
    frac <- option "" (char '.' >> many1 digit)
    return $ JNumber . read $ int ++ "." ++ frac


parseExpr :: Parser JValue
parseExpr  = parseString
         <|> parseNumber
         <|> parseObject
         <|> parseArray
         <|> parseBool
         <|> parseNull

parseArray :: Parser JValue
parseArray = do
  char '['
  x <- sepBy parseExpr ((optional spaces) <* char ',' <* (optional spaces))
  char ']'
  return $ JArray x

parseObject :: Parser JValue
parseObject = do 
  char '{'
  x <- sepBy parseField ((optional spaces) <* char ',' <* (optional spaces))
  char '}'
  return $ JObject x

parseField :: Parser (String, JValue)
parseField = do
  char '"'
  key <- many (noneOf "\"")
  char '"'
  char ':' <* (optional spaces)
  val <- parseExpr
  return $ (,) key val


readJValue :: String -> JValue
readJValue input = case parse parseExpr "json" input of
    Left err -> JString $ "No match: " ++ show err
    Right val -> val


-- accessors
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n)  = Just (truncate n)
getInt _            = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber d) = Just d
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing   
              
getArray :: JValue -> Maybe [JValue]
getArray (JArray a)  = Just a
getArray _           = Nothing