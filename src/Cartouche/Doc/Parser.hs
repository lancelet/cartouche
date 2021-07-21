{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- |
module Cartouche.Doc.Parser where

import Cartouche.Doc.Types
  ( Block (BlockHead, BlockPara),
    Doc (Doc),
    Emph (Emph),
    Head (Head),
    HeadLevel (HeadLevel1, HeadLevel2, HeadLevel3),
    Inline (InlineEmph, InlineStr),
    Para (Para),
    Str (Str),
  )
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import Prelude hiding (head, lines)

-------------------------------------------------------------------------------
-- PARSER TYPE
-------------------------------------------------------------------------------

type Parser = MP.Parsec Void Text

-------------------------------------------------------------------------------
-- DOCUMENT
-------------------------------------------------------------------------------

doc :: Parser Doc
doc = Doc <$> MP.many (block <* optwsnl)

block :: Parser Block
block =
  (BlockHead <$> head)
    <|> (BlockPara <$> para)

cartouche :: Parser a -> Parser a
cartouche p = MP.single '|' *> MP.lookAhead (MP.single '(') *> p

head :: Parser Head
head = cartouche sHead

para :: Parser Para
para = Para <$> inlines

inlines :: Parser [Inline]
inlines = collapseMultipleSpaces . collapseStrs . mconcat <$> MP.some line

collapseStrs :: [Inline] -> [Inline]
collapseStrs xs =
  case xs of
    (InlineStr a) : (InlineStr b) : ys ->
      InlineStr (a <> Str " " <> b) : collapseStrs ys
    y : ys -> y : collapseStrs ys
    [] -> []

collapseMultipleSpaces :: [Inline] -> [Inline]
collapseMultipleSpaces xs = cmsInline <$> xs
  where
    cmsInline :: Inline -> Inline
    cmsInline x =
      case x of
        InlineStr (Str txt) -> InlineStr (Str (cmsText txt))
        InlineEmph (Emph e) -> InlineEmph (Emph (collapseMultipleSpaces e))

    cmsText :: Text -> Text
    cmsText txt = consolidate $ Text.splitOn "  " txt

    consolidate :: [Text] -> Text
    consolidate ys =
      case ys of
        [] -> Text.empty
        "" : "" : zs -> consolidate zs
        "" : zs -> Text.cons ' ' $ consolidate zs
        z : zs -> z <> consolidate zs

line :: Parser [Inline]
line = MP.some inline <* optws <* newLine

inline :: Parser Inline
inline =
  (InlineStr <$> str)
    <|> (InlineEmph <$> emph)

str :: Parser Str
str = Str . mconcat <$> MP.some strChunk
  where
    strChunk, plainStr, escaped :: Parser Text
    strChunk = plainStr <|> escaped
    plainStr = MP.takeWhile1P (Just "plain text") isPlainTextChar
    escaped =
      MP.single '\\'
        *> ( (MP.single '|' $> "|")
               <|> (MP.anySingle >>= \c -> pure $ Text.pack ['\\', c])
           )

emph :: Parser Emph
emph = cartouche sEmph

-------------------------------------------------------------------------------
-- S-EXPRESSIONS
-------------------------------------------------------------------------------

sInlines :: Parser [Inline]
sInlines = MP.many (sInline <* optwsnl)

sInline :: Parser Inline
sInline =
  (InlineStr <$> sStr)
    <|> (InlineEmph <$> sEmph)

sHead :: Parser Head
sHead = inS $ Head <$> (sHeadLevel <* optwsnl) <*> sInlines

sHeadLevel :: Parser HeadLevel
sHeadLevel =
  MP.single 'h'
    *> ( (MP.single '1' $> HeadLevel1)
           <|> (MP.single '2' $> HeadLevel2)
           <|> (MP.single '3' $> HeadLevel3)
       )

sEmph :: Parser Emph
sEmph = inS $ Emph <$> (sSym "emph" *> sInlines)

inS :: Parser a -> Parser a
inS p = MP.single '(' *> optwsnl *> p <* optwsnl <* MP.single ')'

sSym :: Text -> Parser ()
sSym symbolName = MP.chunk symbolName *> optwsnl

sStr :: Parser Str
sStr = Str <$> sString

sString :: Parser Text
sString = MP.single '"' *> strContent <* MP.single '"'
  where
    strContent, strChunk, strReg, strEsc :: Parser Text
    strContent = mconcat <$> MP.many strChunk
    strChunk = strReg <|> strEsc
    strReg = MP.takeWhile1P (Just "regular string character") isRegStringChar
    strEsc =
      MP.single '\\'
        *> ( (MP.single 'n' $> "\n")
               <|> (MP.single '"' $> "\"")
               <|> (MP.single '\\' $> "\\\\")
           )

-------------------------------------------------------------------------------
-- GENERIC PARSERS
-------------------------------------------------------------------------------

blankLine :: Parser ()
blankLine = optws *> newLine $> ()

newLine :: Parser ()
newLine = MP.single '\n' $> ()

optws :: Parser ()
optws = MP.takeWhileP (Just "optional whitespace") isWS $> ()

optwsnl :: Parser ()
optwsnl =
  MP.takeWhileP
    (Just "optional whitespace or newline")
    (\c -> isWS c || isNewLine c)
    $> ()

-------------------------------------------------------------------------------
-- CHARACTER CLASSES
-------------------------------------------------------------------------------

isPlainTextChar :: Char -> Bool
isPlainTextChar c = c /= '\n' && c /= '|' && c /= '\\'

isRegStringChar :: Char -> Bool
isRegStringChar c = c /= '"' && c /= '\\'

isNewLine :: Char -> Bool
isNewLine = (==) '\n'

isWS :: Char -> Bool
isWS = (==) ' '
