-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Token (Token(..), Format(..), Incrementable(..), promote) where

import Roman (Roman(..))

import Data.Char (chr, ord)

data Token =
  TokenDouble Double |
  TokenInt Int |
  TokenLetter Char |
  TokenRoman Int
  deriving (Eq, Ord)

data Format =
  FormatDefault |
  FormatRoman |
  FormatAlpha |
  FormatArabic |
  FormatDouble

instance Show Token where
  show (TokenDouble t) = show t
  show (TokenInt t) = show t
  show (TokenLetter t) = [t]
  show (TokenRoman t) = show (Roman t)

class Incrementable a where
  increase :: a -> a

instance Incrementable Token where
  increase (TokenDouble d) = TokenDouble (d + 1.0)  
  increase (TokenInt i) = TokenInt (i + 1)
  increase (TokenLetter c) = TokenLetter (chr (ord c + 1))
  increase (TokenRoman i) = TokenRoman (i + 1)

promote :: (Token, Token) -> (Token, Token)
promote (t1@(TokenDouble _), t2@(TokenDouble _)) = (t1, t2)
promote (t1@(TokenInt _), t2@(TokenInt _)) = (t1, t2)
promote (t1@(TokenDouble _), TokenInt i2) = (t1, TokenDouble $ fromIntegral i2)
promote (TokenInt i1, t2@(TokenDouble _)) = (TokenDouble $ fromIntegral i1, t2)
promote (t1@(TokenLetter _), t2@(TokenLetter _)) = (t1, t2)
promote (t1@(TokenRoman _), t2@(TokenRoman _)) = (t1, t2)
promote _ = error "start and end must be of compatible type"
