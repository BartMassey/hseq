-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module Roman (Roman(..), toRoman, fromRoman)
where
       
newtype Roman = Roman Int

instance Show Roman where
  show (Roman n) = toRoman n

instance Read Roman where
  readsPrec _ s = [(Roman (fromRoman s), "")]

toRoman :: Int -> String
toRoman n
  | n <= 0 = error "only positive integers are representable as roman numerals"
  | n < 4 = replicate n 'I'
  | n < 5 = "IV"
  | n < 9 = "V" ++ toRoman (n - 5)
  | n < 10 = "IX"
  | n < 40 = let d = n `div` 10 in
             replicate d 'X' ++ toRoman (n - 10 * d)
  | n < 50 = "XL" ++ toRoman (n - 40)
  | n < 90 = let d = (n - 50) `div` 10 in
             "L" ++ replicate d 'X' ++ toRoman (n - 50 - 10 * d)
  | n < 100 = "XC" ++ toRoman (n - 90)
  | n < 400 = let d = n `div` 100 in
             replicate d 'C' ++ toRoman (n - 100 * d)
  | n < 500 = "CD" ++ toRoman (n - 400)
  | n < 900 = let d = (n - 500) `div` 100 in
             "D" ++ replicate d 'C' ++ toRoman (n - 500 - 100 * d)
  | n < 1000 = "CM" ++ toRoman (n - 900)
  | otherwise = let d = n `div` 1000 in
                replicate d 'M' ++ toRoman (n - 1000 * d)

fromRoman :: String -> Int
fromRoman ('M' : ds) = 1000 + fromRoman ds
fromRoman ('D' : ds) = 500 + fromRoman ds
fromRoman ('C' : 'D' : ds) = 400 + fromRoman ds
fromRoman ('C' : 'M' : ds) = 900 + fromRoman ds
fromRoman ('C' : ds) = 100 + fromRoman ds
fromRoman ('L' : ds) = 50 + fromRoman ds
fromRoman ('X' : 'L' : ds) = 40 + fromRoman ds
fromRoman ('X' : 'C' : ds) = 90 + fromRoman ds
fromRoman ('X' : ds) = 10 + fromRoman ds
fromRoman "IV" = 4
fromRoman "IX" = 9
fromRoman ('V' : ds) = 5 + fromRoman ds
fromRoman ('I' : ds) = 1 + fromRoman ds
fromRoman "" = 0
fromRoman (d : _) = error $ "illegal digit " ++ [d] ++ " in roman numeral"
