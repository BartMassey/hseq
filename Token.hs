-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Token (Token(..), Incrementable(..), comparable) where

import Data.Char

data Token =
  TokenDouble Double |
  TokenInt Int |
  TokenLower Char |
  TokenUpper Char
  deriving (Eq, Ord)

instance Show Token where
  show (TokenDouble t) = show t
  show (TokenInt t) = show t
  show (TokenLower t) = [t]
  show (TokenUpper t) = [t]

class Incrementable a where
  increase :: a -> a

instance Incrementable Token where
  increase (TokenDouble d) = TokenDouble (d + 1.0)  
  increase (TokenInt i) = TokenInt (i + 1)
  increase (TokenLower c) = TokenLower (chr (ord c + 1))
  increase (TokenUpper c) = TokenUpper (chr (ord c + 1))

comparable :: Token -> Token -> Bool
comparable (TokenDouble _) (TokenDouble _) = True
comparable (TokenInt _) (TokenInt _) = True
comparable (TokenLower _) (TokenLower _) = True
comparable (TokenUpper _) (TokenUpper _) = True
comparable _ _ = False
