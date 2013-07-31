-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

module SPrintf (sPrintf)
where

import Token

-- | This function emulates C sprintf() on a single
-- `Token` argument. Since some formats are not supported
-- by `Text.Printf` and it is not extensible to new formats,
-- 
sPrintf :: String -> Token -> String
sPrintf _ _ = error "not yet implemented"
