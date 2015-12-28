module TestIO (tests) where

import JSONClass
import SimpleJson
import Pretty

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

-- | main is not really required but can be used to run this test module from GHCi
main :: IO ()
main = defaultMain tests

-- | the list of testcases defined by this test module, will be picked up and executed by TestSuite
tests :: [Test]
tests =
  [
    testCase "check writeFile / readFile cycle" writeAndReadFile,
    testCase "read large JSON File" readJsonFile
  ]

-- example instances
j1 = JNull
j2 = JNumber 1234.123456789
j21 = JNumber 123456
j3 = JString "Hello World"
j4 = JBool True
j5 = toJValue $ JObj [("id", j21), ("text", j3)]
j6 = toJValue $ JAry [j1, j2, j3, j4, j5]
j7 = toJValue $ JAry [j3,j6,j5]


writeAndReadFile = do
    writeFile "test.json" (show (pp j7))
    input <- readFile "test.json"
    -- putStrLn input
    let j8 = readJValue input
    -- putStrLn (show (pp j8))
    assertEqual "check writeFile / readFile cycle:" j7 j8

readJsonFile = do
    input <- readFile "users.json"
    let j8 = readJValue input
    putStrLn (show (pp j8))
    writeFile "users.new.json" (show (pp j8))
    input2 <- readFile "users.new.json"
    let j9 = readJValue input2
    putStrLn "todo: fix this testcase!"
    --assertEqual "check read/write for large file" j8 j9


