{-# LANGUAGE OverloadedStrings #-}

-- |
module Cartouche.Doc where

import Data.Functor (($>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

-------------------------------------------------------------------------------
-- DOCUMENT TREE TYPES
-------------------------------------------------------------------------------

newtype Doc = Doc [Block]

data Block
  = Para [Inline]
  | Heading HeadLevel [Inline]
  deriving (Show)

data HeadLevel = HeadLevel1 | HeadLevel2 | HeadLevel3
  deriving (Show)

data Inline
  = Str !Text
  | Emph [Inline]
  deriving (Show)

-------------------------------------------------------------------------------
-- PARSING DOCUMENT TREE
-------------------------------------------------------------------------------

-- | Parse a document.
--
-- >>> let doc = "|(h1 \"Heading\")\n\nSome |(emph \"it\") text."
-- >>> MP.parseTest pDoc doc
-- [Heading HeadLevel1 [Str "Heading"],Para [Str "Some ",Emph [Str "it"],Str " text."]]
pDoc = pBlock `MP.sepBy` (MP.single '\n' *> pOptWhitespace)

-- | Parse a block.
pBlock :: Parser Block
pBlock = pCartouche pHeading <|> pPara

-- | Parser a paragraph.
--
-- >>> MP.parseTest pPara "This is some |(emph \"emphasized\") text."
-- Para [Str "This is some ",Emph [Str "emphasized"],Str " text."]
--
-- >>> MP.parseTest pPara "one\ntwo\n"
-- Para [Str "one two "]
pPara :: Parser Block
pPara = Para <$> MP.some pParaInline
  where
    pParaInline :: Parser Inline
    pParaInline = (Str <$> pParaChunks) <|> (pCartouche pInline)

    pParaChunks :: Parser Text
    pParaChunks = mconcat <$> MP.some pParaChunk

    pParaChunk :: Parser Text
    pParaChunk = pInlineText <|> pSingleNewline

    pInlineText :: Parser Text
    pInlineText =
      MP.takeWhile1P
        (Just "inline paragraph text")
        (\c -> c /= '|' && c /= '\n')

    pSingleNewline :: Parser Text
    pSingleNewline =
      MP.single '\n' *> MP.notFollowedBy (MP.single '\n') $> " "

pCartouche :: Parser a -> Parser a
pCartouche p = MP.single '|' *> MP.lookAhead (MP.single '(') *> p

-- | Parse a heading.
--
-- >>> MP.parseTest pHeading "(h1 \"First\")"
-- Heading HeadLevel1 [Str "First"]
--
-- >>> MP.parseTest pHeading "(h1 \"First\" (emph \"Serious\") \"Heading\")"
-- Heading HeadLevel1 [Str "First",Emph [Str "Serious"],Str "Heading"]
pHeading :: Parser Block
pHeading = pInSexp (Heading <$> pHeadLevel <*> pInlines)
  where
    pHeadLevel :: Parser HeadLevel
    pHeadLevel =
      MP.single 'h'
        *> ( MP.single '1' $> HeadLevel1
               <|> MP.single '2' $> HeadLevel2
               <|> MP.single '3' $> HeadLevel3
           )
          <* pOptWhitespace

-- | Parse a list of inlines separated by whitespace.
--
-- >>> MP.parseTest pInlines "\"hello\" \"world\""
-- [Str "hello",Str "world"]
pInlines :: Parser [Inline]
pInlines = pSSome pInline

-- | Parse inline elements.
--
-- >>> MP.parseTest pInline "\"str\""
-- Str "str"
pInline :: Parser Inline
pInline =
  Str <$> pStr
    <|> pEmph

-- | Parse inline emphasis.
--
-- >>> MP.parseTest pEmph "(emph \"really\")"
-- Emph [Str "really"]
pEmph :: Parser Inline
pEmph = pInSexp $ Emph <$> (pSym "emph" *> pInlines)

pSSome :: Parser a -> Parser [a]
pSSome p = p `MP.sepBy1` pWhitespace

-------------------------------------------------------------------------------
-- S-EXPRESSION PARSING
-------------------------------------------------------------------------------

type Parser a = MP.Parsec Void Text a

-- | Wrap a parser in S-expression parentheses.
--
-- When parsing an S-expression tree, this parser provides a wrapper around
-- each S-expression, c]onsuming parentheses and any necessary whitespace.
--
-- >>> let p = pInSexp (pSym "foo" *> pStr)
-- >>> MP.parseTest p "(foo \"test\")"
-- "test"
--
-- >>> let p = pInSexp (pSym "foo" *> pInSexp (pSym "bar" *> pStr))
-- >>> MP.parseTest p "(foo (bar \"test\"))"
-- "test"
pInSexp :: Parser a -> Parser a
pInSexp p =
  MP.single '(' *> pOptWhitespace *> p <* pOptWhitespace <* MP.single ')'

-- | Parse a known symbol in an S-expression.
pSym :: Text -> Parser Text
pSym sym = MP.chunk sym <* pOptWhitespace

-- | Parse a double-quoted string in an S-expression.
pStr :: Parser Text
pStr =
  MP.single '"' *> pInStr <* MP.single '"'
  where
    pInStr, pChunk, pReg, pEsc :: Parser Text
    pInStr = mconcat <$> MP.many pChunk
    pChunk = pReg <|> pEsc
    pReg =
      MP.takeWhile1P
        (Just "non-escaped string characters")
        (\c -> c /= '"' && c /= '\\')
    pEsc =
      MP.single '\\'
        *> ( MP.single 'n' $> "\n"
               <|> MP.single 't' $> "\t"
               <|> MP.single '"' $> "\""
               <|> MP.single '\\' $> "\\"
           )

-------------------------------------------------------------------------------
-- UTILITY PARSING
-------------------------------------------------------------------------------

pWhitespace :: Parser Text
pWhitespace = MP.takeWhile1P (Just "whitespace") isWhitespace

pOptWhitespace :: Parser Text
pOptWhitespace = MP.takeWhileP (Just "optional whitespace") isWhitespace

-------------------------------------------------------------------------------
-- CHARACTER CLASSES
-------------------------------------------------------------------------------

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\t' || c == '\n'

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Text.Megaparsec as MP
