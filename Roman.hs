-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Roman (Roman(..), toRoman, fromRoman)
where
       
import qualified Text.Numeral.Roman as R (toRoman, fromRoman)

newtype Roman = Roman Int

instance Show Roman where
  show (Roman n) = toRoman n

instance Read Roman where
  readsPrec _ s = [(Roman (fromRoman s), "")]

toRoman :: Int -> String
toRoman n | n < 0 = "internal error: negative roman numeral"
toRoman 0 = "internal error: zero roman numeral"
toRoman n = R.toRoman n

fromRoman :: String -> Int
fromRoman s =
  case R.fromRoman s of
    Nothing -> error $ "illegal roman numeral " ++ s
    Just x -> x

