--- Copyright © 2013 Bart Massey
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

{
module Lex(lexToken) where

import Data.Word
import Data.ByteString as BS (ByteString, uncons, length, null)
import Data.ByteString.Char8 (pack)

import Token
}

$digit = 0-9
$lower = a-z
$upper = A-Z
@int = \-?$digit+
@double = \-?(\.$digit+|$digit+\.$digit*)(e\-?$digit+)?

tokens :-
  @double { \s -> TokenDouble $ negateableRead $ fixupDouble s }
  @int { \s -> TokenInt $ negateableRead s }
  $lower { \[s] -> TokenLetter s }

{

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

type AlexInput = ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte bs = uncons bs

alexInputPrevChar :: AlexInput -> char
alexInputPrevChar _ = undefined

lexToken :: String -> Token
lexToken s =
  let bs = pack s in
  case alexScan bs 0 of
    AlexEOF -> error "empty value"
    AlexError _ -> error $ "could not parse " ++ s
    AlexSkip _ _ -> error "internal error: unexpected skip"
    AlexToken bs' n act | BS.null bs' && n == BS.length bs -> act s
    AlexToken _ _ _ -> error $ "garbaged value " ++ s

}
