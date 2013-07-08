-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Token (Case(..), Token(..), Format(..), Incrementable(..), 
              promote, promote3)
where

import Roman (Roman(..))

import Data.Char (chr, ord, toUpper, toLower)

data Case = CaseUpper | CaseLower deriving (Eq, Ord)

data Token =
  TokenDouble Double |
  TokenInt Integer |
  TokenAlpha Case Char |
  TokenRoman Case Int
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
  show (TokenAlpha CaseUpper t) = [toUpper t]
  show (TokenAlpha CaseLower t) = [toLower t]
  show (TokenRoman CaseUpper t) = show (Roman t)
  show (TokenRoman CaseLower t) = map toLower $ show (Roman t)

class Incrementable a where
  increase :: a -> a

instance Incrementable Token where
  increase (TokenDouble d) = TokenDouble (d + 1.0)  
  increase (TokenInt i) = TokenInt (i + 1)
  increase (TokenAlpha xc c) = TokenAlpha xc (chr (ord c + 1))
  increase (TokenRoman xc i) = TokenRoman xc (i + 1)

promoteL :: [Token] -> [Token]
promoteL ts
  | all (\x -> case x of TokenAlpha CaseUpper _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenAlpha CaseLower _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenRoman CaseUpper _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenRoman CaseLower _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenInt _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenDouble _ -> True; _ -> False) ts = ts
  | otherwise =
      case promote1 ts of
        Just ts' -> ts'
	Nothing -> error "terms must be of compatible type"
  where
    promote1 [] =
      Just []
    promote1 (TokenInt t : ps) =
      fmap (TokenDouble (fromIntegral t) :) (promote1 ps)
    promote1 (TokenDouble t : ps) =
      fmap (TokenDouble t :) (promote1 ps)
    promote1 _ = Nothing

promote :: (Token, Token) -> (Token, Token)
promote (t1, t2) =
  let [t1', t2'] = promoteL [t1, t2] in (t1', t2')

promote3 :: (Token, Token, Token) -> (Token, Token, Token)
promote3 (t1, t2, t3) =
  let [t1', t2', t3'] = promoteL [t1, t2, t3] in (t1', t2', t3')
