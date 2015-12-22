module Pretty where

import Text.PrettyPrint.Leijen

import SimpleJson

pp :: JValue -> Doc
pp (JString s)    = string s
pp (JNumber n)    = double n
pp (JBool True)   = text "true"
pp (JBool False)  = text "false"
pp (JNull)        = text "null"
pp (JArray arr)   = encloseSep  (char '[') (char ']') (char ',') (map pp arr)
pp (JObject obj)  = encloseSep (char '{') (char '}') (char ',')  (map field obj)
    where field (name,val) = string name
                          <> text ": "
                          <> pp val


