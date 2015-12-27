module Pretty where

import Text.PrettyPrint.Leijen

import JSONClass



pp :: JValue -> Doc
pp (JString s)    = dquotes (string s)
pp (JNumber n)    = double n
pp (JBool True)   = text "true"
pp (JBool False)  = text "false"
pp (JNull)        = text "null"
pp (JArray arr)   = encloseSep  (char '[') (char ']') (char ',') (map pp (fromJAry arr))
pp (JObject obj)  = encloseSep (char '{') (char '}') (char ',')  (map field (fromJObj obj))
    where field (name,val) = dquotes (string name)
                          <> text ": "
                          <> pp val


