-- | Main test runner.
module Main (main) where

import qualified Test.DocTest

main :: IO ()
main = runDocTests

runDocTests :: IO ()
runDocTests = do
  putStrLn "\n---- Running Doctests ----"
  docTests
  putStrLn "\n---- Completed Doctests ----"

docTests :: IO ()
docTests =
  Test.DocTest.doctest
    [ "-isrc",
      "src/Cartouche/Doc/Parser.hs"
    ]
