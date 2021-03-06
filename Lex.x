--- Copyright © 2013 Bart Massey
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

{
-- Suppress warnings from alex-generated code.
-- Thanks https://github.com/jgm/illuminate for the tip.
{-# OPTIONS -w #-}  

module Lex(lexToken) where

import Data.Char (toUpper)
import Data.Word
import Data.ByteString as BS (ByteString, uncons, length, null)
import Data.ByteString.Char8 (pack)

import Token
import Roman
}

$digit = 0-9
$lower = a-z
$upper = A-Z
@int = \-?$digit+
@double = \-?(\.$digit+|$digit+\.$digit*)(e\-?$digit+)?

tokens :-
  @double { lexDouble  }
  @int { lexInt  }
  $upper+ { lexAlphaUpper }
  $lower+ { lexAlphaLower }

{

lexDouble :: (Format, String) -> Token
lexDouble (FormatDefault, s) = lexDouble (FormatDouble, s)
lexDouble (FormatDouble, s) = TokenDouble $ negateableRead $ fixupDouble s
lexDouble (_, s) = error $ "bad format for " ++ s

lexInt :: (Format, String) -> Token
lexInt (FormatDefault, s) = lexInt (FormatArabic, s)
lexInt (FormatArabic, s) = TokenInt $ negateableRead s
lexInt (_, s) = error $ "bad format for " ++ s

lexAlphaUpper :: (Format, String) -> Token
lexAlphaUpper (FormatDefault, s)
  | Prelude.length s > 1 && all (`elem` romanChars) s =
      lexAlphaUpper (FormatRoman, s)
  | otherwise =
      lexAlphaUpper (FormatAlpha, s)
lexAlphaUpper (FormatAlpha, [c]) = TokenAlpha CaseUpper c
lexAlphaUpper (FormatRoman, s) = TokenRoman CaseUpper $ fromRoman s
lexAlphaUpper (_, s) = error $ "malformed character format for " ++ s

lexAlphaLower :: (Format, String) -> Token
lexAlphaLower (FormatDefault, s)
  | Prelude.length s > 1 && all ((`elem` romanChars) . toUpper) s =
      lexAlphaLower (FormatRoman, s)
  | otherwise =
      lexAlphaLower (FormatAlpha, s)
lexAlphaLower (FormatAlpha, [c]) = TokenAlpha CaseLower c
lexAlphaLower (FormatRoman, s) = TokenRoman CaseLower $
                                   fromRoman (map toUpper s)
lexAlphaLower (_, s) = error $ "malformed character format for " ++ s

negateableRead :: (Num a, Read a) => String -> a
negateableRead ('-' : s) = negate $ read s
negateableRead s = read s

fixupDouble :: String -> String
fixupDouble s
  | head s == '-' = "-" ++ fixupDouble (tail s)
  | 'e' `elem` s = 
      let (front, back) = break (== 'e') s in
      fixupDouble front ++ back
  | head s == '.' = "0" ++ s
  | last s == '.' = s ++ "0"
  | otherwise = s

type AlexInput = (Format, ByteString)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (f, bs) =
  fmap formatify $ uncons bs
  where
    formatify (c, bs') = (c, (f, bs'))

alexInputPrevChar :: AlexInput -> char
alexInputPrevChar _ = undefined

lexToken :: Format -> String -> Token
lexToken f s =
  let bs = pack s in
  case alexScan (f, bs) 0 of
    AlexEOF -> error "empty value"
    AlexError _ -> error $ "could not parse " ++ s
    AlexSkip _ _ -> error "internal error: unexpected skip"
    AlexToken (f, bs') n act | BS.null bs' && n == BS.length bs -> act (f, s)
    AlexToken _ _ _ -> error $ "garbaged value " ++ s

}
