module Main (main) where

-- qualified import of test cases
import qualified TestPure
import qualified TestIO

import Test.Framework

-- | entry point into the test suite
main :: IO ()
main = defaultMain tests

-- | assembles all test cases from separate test modules
tests :: [Test]
tests =
  [
    testGroup "Pure Test" TestPure.tests,
    testGroup "IO Test" TestIO.tests
  ]



