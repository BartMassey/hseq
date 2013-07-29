-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (intercalate)
import System.Console.ParseArgs

import Token
import Lex

data ArgIndex =
  ArgWords | ArgLines | ArgSep | ArgFormat | ArgStart | ArgEnd
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
     argIndex = ArgSep,
     argAbbr = Just 's',
     argName = Just "separator",
     argData = argDataOptional "string" ArgtypeString, 
     argDesc = "put a specific separator between sequence elements"},
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
     argData = argDataOptional "start" ArgtypeString,
     argDesc = "first element of sequence" },
  Arg {
     argIndex = ArgEnd,
     argAbbr = Nothing,
     argName = Nothing,
     argData = argDataRequired "end" ArgtypeString,
     argDesc = "last element of sequence" } ]

data OutStyle = OutStyleWords | OutStyleLines | OutStyleSep String

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let sepcount = 
        boolc (gotArg argv ArgWords) +
        boolc (gotArg argv ArgLines) +
        boolc (gotArg argv ArgSep)
        where
          boolc True = 1
          boolc False = 0
  when (sepcount > 1)
    (usageError argv "specified multiple output separator styles")
  let outstyle
        | gotArg argv ArgWords = OutStyleWords
        | gotArg argv ArgLines = OutStyleLines
        | gotArg argv ArgSep = OutStyleSep $ getRequiredArg argv ArgSep
        | otherwise = OutStyleLines
  let outformat =
        case outstyle of
          OutStyleWords -> putStrLn . unwords
          OutStyleLines -> putStr . unlines
          OutStyleSep s -> putStrLn . intercalate s
  let format =
        case fmap (map toLower) $ getArg argv ArgFormat of
	  Just "roman" -> FormatRoman
	  Just "alpha" -> FormatAlpha
	  Just "arabic" -> FormatArabic
	  Just "double" -> FormatDouble
	  Just s -> usageError argv $ "unknown sequence format " ++ s
	  Nothing -> FormatDefault
  let end = lexToken format $ getRequiredArg argv ArgEnd
  let (cf, start) = 
        case getArg argv ArgStart of 
          Just s -> 
            ((<=), lexToken format s)
          Nothing ->
            case end of
              TokenDouble _ -> ((<), TokenDouble 0)
              TokenInt _ -> ((<), TokenInt 0)
              _ -> usageError argv "start value required for this type"
  let (start', end') = promote (start, end)
  outformat $ map show $ takeWhile (`cf` end') $ iterate increase start'
  return ()
