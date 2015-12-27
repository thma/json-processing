module Main (main) where
import qualified TestPure

import Test.Framework

-- | entry point into the test suite
main :: IO ()
main = defaultMain tests

-- | assembles all test cases from separate test modules
tests :: [Test]
tests =
  [
    testGroup "Pure Test" TestPure.tests
  ]



