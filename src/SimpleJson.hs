module SimpleJson where

import Text.ParserCombinators.Parsec -- hiding (spaces)

import JSONClass

-- parser
-- spaces :: Parser ()
-- spaces = skipMany1 space

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
parseNumber = do
    int <- many1 digit
    frac <- option "" (char '.' >> many1 digit)
    return $ JNumber . read $ int ++ "." ++ frac


parseExpr :: Parser JValue
parseExpr =  parseString
         <|> parseNumber
         <|> parseObject
         <|> parseArray
         <|> parseBool
         <|> parseNull

parseArray :: Parser JValue
parseArray = do
  spaces
  char '['
  spaces
  x <- sepBy parseExpr (spaces <* char ',' )
  spaces
  char ']'
  spaces
  return $ toJValue $ JAry x

parseObject :: Parser JValue
parseObject = do 
  spaces
  char '{'
  spaces
  x <- sepBy parseField (spaces <* char ',' <* spaces)
  spaces
  char '}'
  spaces
  return $ toJValue $ JObj x

parseField :: Parser (String, JValue)
parseField = do 
  char '"'
  key <- many (noneOf "\"")
  char '"'
  char ':' <* spaces
  (,) key <$> parseExpr


readJValue :: String -> JValue
readJValue input = case parse parseExpr "json" input of
  Left err -> JString $ "No match: " ++ show err
  Right val -> val

{- we don't need these in an open world...
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
getObject (JObject o) = Just $ fromJObj o
getObject _           = Nothing   
              
getArray :: JValue -> Maybe [JValue]
getArray (JArray a)  = Just $ fromJAry a
getArray _           = Nothing
-}