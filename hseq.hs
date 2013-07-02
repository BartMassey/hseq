-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Control.Monad (when)
import Data.Char (toLower)
import System.Console.ParseArgs

import Token
import Lex

data ArgIndex =
  ArgWords | ArgLines | ArgFormat | ArgStart | ArgEnd
  deriving (Ord, Eq, Show, Enum)

argd :: [Arg ArgIndex]
argd = [
  Arg {
     argIndex = ArgWords,
     argAbbr = Just 'w',
     argName = Just "words",
     argData = Nothing,
     argDesc = "output a wide sequence of words" },
  Arg {
     argIndex = ArgLines,
     argAbbr = Just 'l',
     argName = Just "lines",
     argData = Nothing,
     argDesc = "output a long sequence of lines" },
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

data OutStyle = OutStyleWords | OutStyleLines

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  when (gotArg argv ArgWords && gotArg argv ArgLines)
    (error "specified both words and lines as output style")
  let outstyle
        | gotArg argv ArgWords = OutStyleWords
        | gotArg argv ArgLines = OutStyleLines
        | otherwise = OutStyleLines
  let outformat =
        case outstyle of
          OutStyleWords -> putStrLn . unwords
          OutStyleLines -> putStr . unlines
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
  let (start', end') = promote (start, end)
  outformat $ map show $ takeWhile (<= end') $ iterate increase start'
  return ()
