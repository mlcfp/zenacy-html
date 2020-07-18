--------------------------------------------------------------------------------
-- Copyright (C) 2016 Zenacy Reader Technologies LLC
--------------------------------------------------------------------------------

module Zenacy.HTML.Internal.Char
  ( ctow
  , chrWord8
  , chrUTF8
  , chrSurrogate
  , chrScalar
  , chrNonCharacter
  , chrASCIIDigit
  , chrASCIIUpperHexDigit
  , chrASCIILowerHexDigit
  , chrASCIIHexDigit
  , chrASCIIUpperAlpha
  , chrASCIILowerAlpha
  , chrASCIIAlpha
  , chrASCIIAlphanumeric
  , chrWhitespace
  , chrC0Control
  , chrControl
  , chrToUpper
  , chrToLower
  , chrAmpersand
  , chrEOF
  , chrExclamation
  , chrGreater
  , chrLess
  , chrQuestion
  , chrSolidus
  , chrTab
  , chrLF
  , chrFF
  , chrCR
  , chrSpace
  , chrEqual
  , chrQuote
  , chrApostrophe
  , chrGrave
  , chrNumberSign
  , chrHyphen
  , chrBracketRight
  , chrSemicolon
  , chrUpperX
  , chrLowerX
  ) where

import qualified Data.ByteString as S
  ( unpack
  )
import Data.Char
  ( chr
  , ord
  )
import qualified Data.Text as Text
  ( singleton
  )
import qualified Data.Text.Encoding as Text
  ( encodeUtf8
  )
import Data.Word
  ( Word8
  )
import Data.Word8

ctow :: Char -> Word8
ctow x = fromIntegral (ord x)

-- | Determines if a character code is in the range of a Word8.
chrWord8 :: Int -> Bool
chrWord8 x = x >= 0 && x <= 0xFF

chrUTF8 :: Int -> [Word8]
chrUTF8 = S.unpack . Text.encodeUtf8 . Text.singleton . chr

-- | Determines if a character code is a surrogate.
chrSurrogate :: Int -> Bool
chrSurrogate x = x >= 0xD800 && x <= 0xDFFF

-- | Determines if a character code is a scalar.
chrScalar :: Int -> Bool
chrScalar = not . chrSurrogate

-- | Determines if a code is a not a character code.
chrNonCharacter :: Int -> Bool
chrNonCharacter x =
  (x >= 0xFDD0 && x <= 0xFDEF) ||
  any (==x)
    [ 0xFFFE, 0xFFFF, 0x1FFFE, 0x1FFFF
    , 0x2FFFE, 0x2FFFF, 0x3FFFE, 0x3FFFF
    , 0x4FFFE, 0x4FFFF, 0x5FFFE, 0x5FFFF
    , 0x6FFFE, 0x6FFFF, 0x7FFFE, 0x7FFFF
    , 0x8FFFE, 0x8FFFF, 0x9FFFE, 0x9FFFF
    , 0xAFFFE, 0xAFFFF, 0xBFFFE, 0xBFFFF
    , 0xCFFFE, 0xCFFFF, 0xDFFFE, 0xDFFFF
    , 0xEFFFE, 0xEFFFF, 0xFFFFE, 0xFFFFF
    , 0x10FFFE, 0x10FFFF
    ]

chrASCIIDigit :: Word8 -> Bool
chrASCIIDigit x = x >= 0x30 && x <= 0x39

chrASCIIUpperHexDigit :: Word8 -> Bool
chrASCIIUpperHexDigit x = chrASCIIDigit x || (x >= 0x41 && x <= 0x46)

chrASCIILowerHexDigit :: Word8 -> Bool
chrASCIILowerHexDigit x = chrASCIIDigit x || (x >= 0x61 && x <= 0x66)

chrASCIIHexDigit :: Word8 -> Bool
chrASCIIHexDigit x = chrASCIIUpperHexDigit x || chrASCIILowerHexDigit x

chrASCIIUpperAlpha :: Word8 -> Bool
chrASCIIUpperAlpha x = x >= 0x41 && x <= 0x5A

chrASCIILowerAlpha :: Word8 -> Bool
chrASCIILowerAlpha x = x >= 0x61 && x <= 0x7A

chrASCIIAlpha :: Word8 -> Bool
chrASCIIAlpha x = chrASCIIUpperAlpha x || chrASCIILowerAlpha x

chrASCIIAlphanumeric :: Word8 -> Bool
chrASCIIAlphanumeric x = chrASCIIDigit x || chrASCIIAlpha x

chrWhitespace :: Word8 -> Bool
chrWhitespace x =
  x == chrTab ||
  x == chrLF ||
  x == chrFF ||
  x == chrCR ||
  x == chrSpace

chrC0Control :: Word8 -> Bool
chrC0Control x = x >= 0x00 && x <= 0x1F

chrControl :: Word8 -> Bool
chrControl x = chrC0Control x || (x >= 0x7F && x <= 0x9F)

chrToUpper :: Word8 -> Word8
chrToUpper = toUpper

chrToLower :: Word8 -> Word8
chrToLower = toLower

chrAmpersand :: Word8
chrAmpersand = _ampersand

chrEOF :: Word8
chrEOF = _nul

chrExclamation :: Word8
chrExclamation = _exclam

chrGreater :: Word8
chrGreater = _greater

chrLess :: Word8
chrLess = _less

chrQuestion :: Word8
chrQuestion = _question

chrSolidus :: Word8
chrSolidus = _slash

chrTab :: Word8
chrTab = _tab

chrLF :: Word8
chrLF = _lf

chrFF :: Word8
chrFF = _np

chrCR :: Word8
chrCR = _cr

chrSpace :: Word8
chrSpace = _space

chrEqual :: Word8
chrEqual = _equal

chrQuote :: Word8
chrQuote = _quotedbl

chrApostrophe :: Word8
chrApostrophe = _quotesingle

chrGrave :: Word8
chrGrave = _grave

chrNumberSign :: Word8
chrNumberSign = _numbersign

-- | Character code for a hyphen.
chrHyphen :: Word8
chrHyphen = _hyphen

-- | Character code for a right bracket.
chrBracketRight :: Word8
chrBracketRight = _bracketright

chrSemicolon :: Word8
chrSemicolon = _semicolon

chrUpperX :: Word8
chrUpperX = 0x58

chrLowerX :: Word8
chrLowerX = 0x78
