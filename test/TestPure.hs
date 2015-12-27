module TestPure (tests) where

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
    testCase "check read / pp cyle:" (assertEqual "check read / pp cyle:" j7 (readJValue (show (pp j7))))
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

{-
main = do
    putStrLn $ show $ pp j7
    putStrLn $ show $ pp (readJValue (show (pp j7)))
    putStrLn $ "check read / pp cyle: " ++ show (j7 == readJValue (show (pp j7)))

-}
