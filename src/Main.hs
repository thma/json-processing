module Main where
import JSONClass
import SimpleJson
import Pretty

-- example instances
j1 = JNull
j2 = JNumber 1234.123456789
j21 = JNumber 123456
j3 = JString "Hello World"
j4 = JBool True
j5 = toJValue $ JObj [("id", j21), ("text", j3)]
j6 = toJValue $ JAry [j1, j2, j3, j4, j5]
j7 = toJValue $ JAry [j3,j6,j5]


main = do
    -- writeFile "test.json" (show (pp j7))
    input <- readFile "users.json"
    putStrLn input
    j8 <- return (readJValue input)
    putStrLn (show (pp j8))
