-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Control.Monad
import System.Console.ParseArgs

import Token
import Lex

data ArgIndex = ArgStart | ArgEnd deriving (Ord, Eq, Show, Enum)

argd :: [Arg ArgIndex]
argd = [
  Arg {
     argIndex = ArgStart,
     argAbbr = Nothing,
     argName = Nothing,
     argData = argDataRequired "start" ArgtypeString,
     argDesc = "first element of sequence"
  },
  Arg {
     argIndex = ArgEnd,
     argAbbr = Nothing,
     argName = Nothing,
     argData = argDataRequired "end" ArgtypeString,
     argDesc = "last element of sequence"
  } ]

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let start = lexToken $ getRequiredArg argv ArgStart
  let end = lexToken $ getRequiredArg argv ArgEnd
  unless (comparable start end) (error "start and end must be same type")
  putStr $ unlines $ map show $ takeWhile (<= end) $ iterate increase start
  return ()
