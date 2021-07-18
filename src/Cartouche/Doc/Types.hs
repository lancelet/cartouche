-- | Types for documents.
module Cartouche.Doc.Types where

import Data.Text (Text)
import Prelude hiding (Word)

newtype Doc = Doc [Block]
  deriving (Show)

data Block
  = BlockPara !Para
  | BlockHead !Head
  deriving (Show)

newtype Para = Para [Inline]
  deriving (Show)

data Inline
  = InlineStr !Str
  | InlineEmph !Emph
  deriving (Show)

data Head = Head !HeadLevel [Inline]
  deriving (Show)

data HeadLevel = HeadLevel1 | HeadLevel2 | HeadLevel3
  deriving (Show)

newtype Emph = Emph [Inline]
  deriving (Show)

newtype Word = Word Text
  deriving (Show)

newtype Str = Str Text
  deriving (Show)
