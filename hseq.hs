-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Control.Monad
import Data.Char (toLower)
import System.Console.ParseArgs

import Token
import Lex

data ArgIndex =
  ArgFormat | ArgStart | ArgEnd
  deriving (Ord, Eq, Show, Enum)

argd :: [Arg ArgIndex]
argd = [
  Arg {
     argIndex = ArgFormat,
     argAbbr = Just 'f',
     argName = Just "format",
     argData = argDataOptional "format" ArgtypeString,
     argDesc = "sequence element format" },
  Arg {
     argIndex = ArgStart,
     argAbbr = Nothing,
     argName = Nothing,
     argData = argDataRequired "start" ArgtypeString,
     argDesc = "first element of sequence" },
  Arg {
     argIndex = ArgEnd,
     argAbbr = Nothing,
     argName = Nothing,
     argData = argDataRequired "end" ArgtypeString,
     argDesc = "last element of sequence" } ]

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let format =
        case fmap (map toLower) $ getArg argv ArgFormat of
	  Just "roman" -> FormatRoman
	  Just "alpha" -> FormatAlpha
	  Just "arabic" -> FormatArabic
	  Just "double" -> FormatDouble
	  Just s -> error $ "unknown sequence format " ++ s
	  Nothing -> FormatDefault
  let start = lexToken format $ getRequiredArg argv ArgStart
  let end = lexToken format $ getRequiredArg argv ArgEnd
  unless (comparable start end) (error "start and end must be same type")
  putStr $ unlines $ map show $ takeWhile (<= end) $ iterate increase start
  return ()
