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
  readsPrec _ = (\s -> [(Roman (fromRoman s), "")])

toRoman :: Int -> String
toRoman n
  | n < 4 = replicate n 'i'
  | n == 4 = "iv"
  | n < 9 = "v" ++ toRoman (n - 5)
  | n == 9 = "ix"
  | n < 40 = let d = n `div` 10 in
             replicate d 'x' ++ toRoman (n - 10 * d)
  | n < 50 = "xl" ++ toRoman (n - 40)
  | n < 90 = let d = (n - 50) `div` 10 in
             "l" ++ replicate d 'x' ++ toRoman (n - 50 - 10 * d)
  | n < 100 = "xc" ++ toRoman (n - 90)
  | n < 400 = let d = n `div` 100 in
             replicate d 'c' ++ toRoman (n - 100 * d)
  | n < 500 = "cd" ++ toRoman (n - 400)
  | n < 900 = let d = (n - 500) `div` 100 in
             "d" ++ replicate d 'c' ++ toRoman (n - 500 - 100 * d)
  | n < 1000 = "cm" ++ toRoman (n - 900)
  | otherwise = let d = n `div` 1000 in
                replicate d 'm' ++ toRoman (n - 1000 * d)

fromRoman :: String -> Int
fromRoman ('m' : ds) = 1000 + fromRoman ds
fromRoman ('d' : ds) = 500 + fromRoman ds
fromRoman ('c' : 'd' : ds) = 400 + fromRoman ds
fromRoman ('c' : 'm' : ds) = 900 + fromRoman ds
fromRoman ('c' : ds) = 100 + fromRoman ds
fromRoman ('l' : ds) = 50 + fromRoman ds
fromRoman ('x' : 'l' : ds) = 40 + fromRoman ds
fromRoman ('x' : 'c' : ds) = 90 + fromRoman ds
fromRoman ('x' : ds) = 10 + fromRoman ds
fromRoman "iv" = 4
fromRoman "ix" = 9
fromRoman ('i' : ds) = 1 + fromRoman ds
fromRoman "" = 0
fromRoman (d : _) = error $ "illegal digit " ++ [d] ++ " in roman numeral"
