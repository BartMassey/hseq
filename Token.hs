-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Token (Token(..), Incrementable(..), comparable) where

import Roman (Roman(..))

import Data.Char (chr, ord)

data Token =
  TokenDouble Double |
  TokenInt Int |
  TokenLetter Char |
  TokenRoman Int
  deriving (Eq, Ord)

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

comparable :: Token -> Token -> Bool
comparable (TokenDouble _) (TokenDouble _) = True
comparable (TokenInt _) (TokenInt _) = True
comparable (TokenDouble _) (TokenInt _) = True
comparable (TokenInt _) (TokenDouble _) = True
comparable (TokenLetter _) (TokenLetter _) = True
comparable (TokenRoman _) (TokenRoman _) = True
comparable _ _ = False
