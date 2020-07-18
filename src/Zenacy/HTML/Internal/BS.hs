--------------------------------------------------------------------------------
-- Copyright (C) 2016 Zenacy Reader Technologies LLC
--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zenacy.HTML.Internal.BS
  ( BS
  -- * Word functions.
  , bsEmpty
  , bsOnly
  , bsLen
  , bsPack
  , bsUnpack
  , bsConcat
  , bsIndex
  , bsElemIndex
  , bsLower
  , bsPrefixCI
  , bsPart
  , bsLast
  , bsTake
  , bsDrop
  , bsUncons
  -- * Character functions.
  , bcPack
  , bcUnpack
  ) where

import Zenacy.HTML.Internal.Char
import Data.ByteString
  ( ByteString
  )
import qualified Data.ByteString as S
  ( concat
  , drop
  , elemIndex
  , empty
  , isPrefixOf
  , index
  , last
  , length
  , map
  , pack
  , singleton
  , take
  , uncons
  , unpack
  )
import qualified Data.ByteString.Char8 as C
  ( pack
  , unpack
  )
import Data.Word
  ( Word8
  )

type BS = ByteString

bsEmpty :: BS
bsEmpty = S.empty
{-# INLINE bsEmpty #-}

bsOnly :: Word8 -> BS
bsOnly = S.singleton
{-# INLINE bsOnly #-}

bsLen :: BS -> Int
bsLen = S.length
{-# INLINE bsLen #-}

bsPack :: [Word8] -> BS
bsPack = S.pack
{-# INLINE bsPack #-}

bsUnpack :: BS -> [Word8]
bsUnpack = S.unpack
{-# INLINE bsUnpack #-}

bsConcat :: [BS] -> BS
bsConcat = S.concat
{-# INLINE bsConcat #-}

bsIndex :: BS -> Int -> Word8
bsIndex = S.index
{-# INLINE bsIndex #-}

bsElemIndex :: Word8 -> BS -> Maybe Int
bsElemIndex = S.elemIndex
{-# INLINE bsElemIndex #-}

-- | Converts a bytestring to lowercase.
bsLower :: BS -> BS
bsLower = S.map chrToLower

-- | Determines if a bytestring is a case-insensitive prefix of another.
bsPrefixCI :: BS -> BS -> Bool
bsPrefixCI x y = bsLower x `S.isPrefixOf` bsLower y

-- | Selects a substring for a byte string.
bsPart :: Int -> Int -> BS -> BS
bsPart offset len = S.take len . S.drop offset

-- | Returns the last word of a bytestring.
bsLast :: BS -> Maybe Word8
bsLast x
  | bsLen x > 0 = Just $ S.last x
  | otherwise = Nothing

bsTake :: Int -> BS -> BS
bsTake = S.take
{-# INLINE bsTake #-}

bsDrop :: Int -> BS -> BS
bsDrop = S.drop
{-# INLINE bsDrop #-}

bsUncons :: BS -> Maybe (Word8, BS)
bsUncons = S.uncons
{-# INLINE bsUncons #-}

bcPack :: String -> BS
bcPack = C.pack
{-# INLINE bcPack #-}

bcUnpack :: BS -> String
bcUnpack = C.unpack
{-# INLINE bcUnpack #-}
