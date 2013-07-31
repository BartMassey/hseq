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
  ArgWords | ArgLines | ArgSep | ArgFormat | 
  ArgStart | ArgEnd  | ArgIncr | 
  ArgWiden | ArgPad | ArgSpacePad
  deriving (Ord, Eq, Show, Enum)

argd :: [Arg ArgIndex]
argd = [
  Arg {
     argIndex = ArgWords,
     argAbbr = Just 'W',
     argName = Just "words",
     argData = Nothing,
     argDesc = "output a wide sequence of words" },
  Arg {
     argIndex = ArgLines,
     argAbbr = Just 'L',
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
     argIndex = ArgWiden,
     argAbbr = Just 'w',
     argName = Just "widen",
     argData = Nothing,
     argDesc = "widen sequence elements to equal width by zero-padding" },
  Arg {
     argIndex = ArgSpacePad,
     argAbbr = Just 'P',
     argName = Just "pad-spaces",
     argData = Nothing,
     argDesc = "widen sequence elements to equal width by space-padding" },
  Arg {
     argIndex = ArgPad,
     argAbbr = Just 'p',
     argName = Just "pad",
     argData = argDataOptional "char" ArgtypeString,
     argDesc = "widen sequence elements to equal width by padding with char" },
  Arg {
     argIndex = ArgStart,
     argAbbr = Nothing,
     argName = Nothing,
     argData = argDataOptional "start" ArgtypeString,
     argDesc = "first element of sequence" },
  Arg {
     argIndex = ArgIncr,
     argAbbr = Nothing,
     argName = Nothing,
     argData = argDataOptional "incr" ArgtypeString,
     argDesc = "sequence increment" },
  Arg {
     argIndex = ArgEnd,
     argAbbr = Nothing,
     argName = Nothing,
     argData = argDataRequired "end" ArgtypeString,
     argDesc = "last element of sequence" } ]

dupArgs :: Args ArgIndex -> [ArgIndex] -> String -> IO ()
dupArgs argv args msg =
  if length (filter (gotArg argv) args) > 1
  then usageError argv msg
  else return ()

main :: IO ()
main = do
  argv <- parseArgsIO (ArgsParseControl ArgsComplete ArgsSoftDash) argd
  -- Handle separators
  dupArgs argv [ArgWords, ArgLines, ArgSep]
    "cannot specify multiple output separator styles"
  let outformat
        | gotArg argv ArgWords =
            putStrLn . unwords
        | gotArg argv ArgSep =
            putStrLn . intercalate (getRequiredArg argv ArgSep)
        | otherwise =
            putStr . unlines
  -- Handle output format
  let format =
        case fmap (map toLower) $ getArg argv ArgFormat of
	  Just "roman" -> FormatRoman
	  Just "alpha" -> FormatAlpha
	  Just "arabic" -> FormatArabic
	  Just "double" -> FormatDouble
	  Just s -> usageError argv $ "unknown sequence format " ++ s
	  Nothing -> FormatDefault
  -- Handle output padding
  dupArgs argv [ArgWiden, ArgPad, ArgSpacePad]
    "cannot specify multiple padding styles"
  let outpad
        | gotArg argv ArgWiden =
            case format of
              FormatRoman -> padWith ' '
              _ -> padWith '0'
        | gotArg argv ArgSpacePad = padWith ' '
        | gotArg argv ArgPad =
            case getRequiredArg argv ArgPad of
              [c] -> padWith c
              _ -> usageError argv "pad must be single character"
        | otherwise = id
        where
          padWith _ [] = []
          padWith c es =
            map padOne es
            where
              maxw = maximum $ map length es
              padOne e = replicate (maxw - length e) c ++ e
  -- Handle sequence specifiers
  let end = lexToken format $ getRequiredArg argv ArgEnd
  let (cfeq, start, incr) =
        let incr' = 
              case getArg argv ArgIncr of
                Nothing -> TokenInt 1
                Just s -> lexToken format s
        in
         case getArg argv ArgStart of 
           Just s -> 
             (True, lexToken format s, incr')
           Nothing ->
             case end of
               TokenDouble _ -> (False, TokenDouble 0, incr')
               TokenInt _ -> (False, TokenInt 0, incr')
               _ -> usageError argv "start value required for this type"
  let cf =
        case incrSign incr of
          IncrZero ->
            usageError argv "increment should never be zero"
          IncrPos ->
            case cfeq of
              True -> (<=)
              False -> (<)
          IncrNeg ->
            case cfeq of
              True -> (>=)
              False -> (>)
  let (start', end', incr') = promote3 (start, end, incr)
  -- Do it
  outformat $ outpad $ map show $ 
    takeWhile (`cf` end') $ iterate (increase incr') start'
  return ()
