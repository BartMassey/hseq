-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Token (Case(..), Token(..), Format(..), IncrSign(..),
              Incrementable(..), promote3)
where

import Roman (Roman(..))

import Data.Char (chr, ord, toUpper, toLower)

data Case = CaseUpper | CaseLower deriving (Eq, Ord, Show)

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
  FormatDouble |
  FormatByString String

instance Show Token where
  show (TokenDouble t) = show t
  show (TokenInt t) = show t
  show (TokenAlpha CaseUpper t) = [toUpper t]
  show (TokenAlpha CaseLower t) = [toLower t]
  show (TokenRoman CaseUpper t) = show (Roman t)
  show (TokenRoman CaseLower t) = map toLower $ show (Roman t)



showType :: Token -> String
showType (TokenInt _) = "TokenInt"
showType (TokenAlpha xc _) = "TokenAlpha/" ++ show xc
showType (TokenDouble _) = "TokenDouble"
showType (TokenRoman xc _) = "TokenRoman/" ++ show xc

data IncrSign = IncrPos | IncrZero | IncrNeg

class Incrementable a where
  increase :: a -> a -> a
  incrSign :: a -> IncrSign

instance Incrementable Token where
  increase (TokenDouble inc) (TokenDouble d) = TokenDouble (d + inc)
  increase (TokenInt inc) (TokenInt i) = TokenInt (i + inc)
  increase (TokenInt inc) (TokenAlpha xc c) = TokenAlpha xc (chr (ord c + fromIntegral inc))
  increase (TokenInt inc) (TokenRoman xc i) = TokenRoman xc (i + fromIntegral inc)
  increase (TokenRoman _ inc) (TokenRoman xc i) = TokenRoman xc (i + inc)
  increase inc base = error $ "internal error: bad promotion " ++
                        "yields illegal increase: " ++ 
                        showType inc ++ " " ++ showType base
  incrSign (TokenDouble v)
    | v > 0 = IncrPos
    | v < 0 = IncrNeg
    | otherwise = IncrZero
  incrSign (TokenInt v)
    | v > 0 = IncrPos
    | v < 0 = IncrNeg
    | otherwise = IncrZero
  incrSign (TokenRoman _ v)
    | v > 0 = IncrPos
    | v < 0 = IncrNeg
    | otherwise = IncrZero
  incrSign (TokenAlpha _ _) =
    IncrZero

promoteL :: [Token] -> [Token]
promoteL [] = error "internal error: promoted empty list"
promoteL ts
  | all (\x -> case x of TokenAlpha CaseUpper _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenAlpha CaseLower _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenRoman CaseUpper _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenRoman CaseLower _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenInt _ -> True; _ -> False) ts = ts
  | all (\x -> case x of TokenDouble _ -> True; _ -> False) ts = ts
  | otherwise =
      case foldr promoteIntToDouble (Just []) ts of
        Just ts' -> ts'
	Nothing -> error "terms must be of compatible type"
  where
    promoteIntToDouble t es =
      let d = 
            case t of
              TokenDouble d' -> Just d'
              TokenInt i -> Just (fromIntegral i)
              _ -> Nothing
      in
         (\v -> return . (TokenDouble v :) =<< es) =<< d

promote3 :: (Token, Token, Token) -> (Token, Token, Token)
promote3 (t1, t2, t3) =
  let [t1', t2'] = promoteL [t1, t2]
      t3' = case (t3, t1') of
        ((TokenInt inc), (TokenDouble _)) -> TokenDouble $ fromIntegral inc
        ((TokenDouble _), (TokenDouble _)) -> t3
        ((TokenInt _), (TokenInt _)) -> t3
        ((TokenInt _),  (TokenAlpha _ _)) -> t3
        ((TokenInt _), (TokenRoman _ _)) -> t3
        ((TokenRoman _ inc), (TokenRoman _ _)) -> TokenInt $ fromIntegral inc
        _ -> error "illegal increment type"
  in
   (t1', t2', t3')
