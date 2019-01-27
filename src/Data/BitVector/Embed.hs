
{-|
Module      : Data.BitVector.Embed
Copyright   : (c) Galois Inc. 2018
License     : BSD-3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module exports types and functions for defining how a small bit vector is
embedded into a larger one.

-}

module Data.BitVector.Embed
  ( BitEmbedding
  , fromList
  , bitEmbed
  , bitExtract
  ) where

import qualified Data.Bits as B

-- | Defines a mapping from each bit of a small bit vector into a larger one.
newtype BitEmbedding = BitEmbedding [Int]
  deriving (Eq, Ord, Show)

-- | Construct a `BitEmbedding` from a list.
fromList :: [Int] -> BitEmbedding
fromList = BitEmbedding

-- | Embed a smaller bit vector into a larger one using a `BitEmbedding`.
bitEmbed :: (B.Bits src, B.Bits tgt) => BitEmbedding -> src -> tgt -> tgt
bitEmbed (BitEmbedding []) _ tgt = tgt
bitEmbed (BitEmbedding (bit:rst)) src tgt =
  bitEmbed (BitEmbedding rst) (src `B.shiftR` 1) (tgt B..|. bitMask)
  where bitMask = if B.testBit src 0
                  then B.bit bit
                  else B.zeroBits

-- | Extract a smaller bit vector from a larger one using a `BitEmbedding`.
bitExtract :: (B.Bits src, B.Bits tgt) => BitEmbedding -> tgt -> src
bitExtract (BitEmbedding []) _ = B.zeroBits
bitExtract (BitEmbedding (bit:rst)) tgt =
  (bitExtract (BitEmbedding rst) tgt `B.shiftL` 1) B..|. bitMask
  where bitMask = if B.testBit tgt bit
                  then B.bit 0
                  else B.zeroBits
