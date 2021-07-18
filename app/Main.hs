module Main where

import qualified Cartouche.Doc.Parser as Parser
import Data.Text.IO (readFile)
import qualified Text.Megaparsec as MP
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Loading test.cart"
  txt <- readFile "test.cart"
  putStrLn "Running parser"
  -- MP.parseTest Parser.doc txt
