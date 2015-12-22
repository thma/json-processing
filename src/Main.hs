module Main where

import SimpleJson
import Pretty

-- example instances
j1 = JNull
j2 = JNumber 1234.123456789
j21 = JNumber 123456
j3 = JString "Hello World"
j4 = JBool True
j5 = JObject [("id", j21), ("text", j3)]
j6 = JArray [j1, j2, j3, j4, j5]
j7 = JArray [j3,j6,j5]


main = do
    putStrLn $ show $ pp j7
    putStrLn $ show $ pp (readJValue (show (pp j7)))
