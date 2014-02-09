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
  ArgWords | ArgLines | ArgSep | 
  ArgFormat | ArgFormatString |
  ArgStart | ArgEnd  | ArgIncr | 
  ArgWiden | ArgPad | ArgSpacePad | 
  ArgWidth | ArgNumber
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
     argIndex = ArgNumber,
     argAbbr = Just 'n',
     argName = Just "number-lines",
     argData = Nothing,
     argDesc = "number lines as a filter" },
  Arg {
     argIndex = ArgSep,
     argAbbr = Just 's',
     argName = Just "separator",
     argData = argDataOptional "string" ArgtypeString, 
     argDesc = "put separator between sequence elements"},
  Arg {
     argIndex = ArgFormat,
     argAbbr = Just 'F',
     argName = Just "format-word",
     argData = argDataOptional "format" ArgtypeString,
     argDesc = "sequence element format as a word" },
  Arg {
     argIndex = ArgFormatString,
     argAbbr = Just 'f',
     argName = Just "format",
     argData = argDataOptional "format" ArgtypeString,
     argDesc = "sequence element format string" },
  Arg {
     argIndex = ArgWiden,
     argAbbr = Just 'w',
     argName = Just "widen",
     argData = Nothing,
     argDesc = "widen elements by zero-padding" },
  Arg {
     argIndex = ArgSpacePad,
     argAbbr = Just 'P',
     argName = Just "pad-spaces",
     argData = Nothing,
     argDesc = "widen elements by space-padding" },
  Arg {
     argIndex = ArgPad,
     argAbbr = Just 'p',
     argName = Just "pad",
     argData = argDataOptional "char" ArgtypeString,
     argDesc = "widen elements by padding with char" },
  Arg {
     argIndex = ArgWidth,
     argAbbr = Just 'z',
     argName = Just "zero-width",
     argData = argDataOptional "width" ArgtypeInt,
     argDesc = "number of characters to pad elements to" },
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
     argData = argDataOptional "end" ArgtypeString,
     argDesc = "last element of sequence" } ]

dupArgs :: Args ArgIndex -> [ArgIndex] -> String -> IO ()
dupArgs argv args msg =
  if length (filter (gotArg argv) args) > 1
  then usageError argv msg
  else return ()

numberLines :: ([String] -> [String]) -> [String] -> IO ()
numberLines outpad lineNumbers = do
  contents <- getContents
  putStr $ unlines $ map joinNum $
    padNumbers $ zip lineNumbers $ lines contents
  where
    padNumbers lns =
      let nums = outpad $ map fst lns
          lines = map snd lns
      in
       zip nums lines
    joinNum (num, line) = num ++ " " ++ line

main :: IO ()
main = do
  argv <- parseArgsIO (ArgsParseControl ArgsComplete ArgsSoftDash) argd
  -- Handle separators
  dupArgs argv [ArgWords, ArgLines, ArgSep, ArgNumber]
    "cannot specify multiple output styles"
  let outformat
        | gotArg argv ArgWords =
            putStrLn . unwords
        | gotArg argv ArgSep =
            putStrLn . intercalate (getRequiredArg argv ArgSep)
        | otherwise =
            putStr . unlines
  -- Handle output format
  dupArgs argv [ArgFormat, ArgFormatString]
    "cannot specify multiple format styles"
  let format =
        case getArg argv ArgFormatString of
          Just s -> FormatByString s
          Nothing ->
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
  when (gotArg argv ArgWidth &&
        not (any (gotArg argv) [ArgWiden, ArgPad, ArgSpacePad]))
      (usageError argv "zero-width specified but no padding")
  let padEngine =
          padWith (getArg argv ArgWidth)
          where
            padWith _ _ [] = []
            padWith maybeWidth c es =
                map padOne es
                where
                  maxw = maximum $ map length es
                  limw = case maybeWidth of
                           Nothing -> maxw
                           Just w -> w `max` maxw
                  padOne e = replicate (limw - length e) c ++ e
  let outpad
        | gotArg argv ArgWiden =
            case format of
              FormatRoman -> padEngine ' '
              _ -> padEngine '0'
        | gotArg argv ArgSpacePad = padEngine ' '
        | gotArg argv ArgPad =
            case getRequiredArg argv ArgPad of
              [c] -> padEngine c
              _ -> usageError argv "pad must be single character"
        | otherwise = id
  -- Handle sequence specifiers
  let (cf, end, start, incr) =
        case gotArg argv ArgNumber of
          True ->
            let end' =
                  if gotArg argv ArgEnd 
                  then usageError argv "cannot give ending line number"
                  else TokenInt (-1)
                (start', incr') =
                  let incr'' = 
                        case getArg argv ArgIncr of
                          Nothing -> TokenInt 1
                          Just s -> lexToken format s
                  in
                   case getArg argv ArgStart of 
                     Just s -> 
                       (lexToken format s, incr'')
                     Nothing ->
                       case end of
                         TokenDouble _ -> (TokenDouble 1, incr'')
                         TokenInt _ -> (TokenInt 1, incr'')
                         _ -> usageError argv
                                "start value required for this type"
            in
             ((\_ _ -> True), end', start', incr')
          False ->
            let (startArg, incrArg, endArg) =
                  let arg1 = getArg argv ArgStart
                      arg2 = getArg argv ArgIncr
                      arg3 = getArg argv ArgEnd
                  in
                   case (arg1, arg2, arg3) of
                     (Nothing, Nothing, Nothing) ->
                       usageError argv "ending value required"
                     (Just a1, Nothing, Nothing) ->
                       (Nothing, Nothing, a1)
                     (Just _, Just a2, Nothing) ->
                       (arg1, Nothing, a2)
                     (Just _, Just _, Just a3) ->
                       (arg1, arg2, a3)
                     _ ->
                       error "internal error: bad parse of limit arguments"
                end' = lexToken format endArg
                (cfeq, start', incr') =
                  let incr'' = 
                        case incrArg of
                          Nothing -> TokenInt 1
                          Just s -> lexToken format s
                  in
                   case startArg of 
                     Just s -> 
                       (True, lexToken format s, incr'')
                     Nothing ->
                       case end' of
                         TokenDouble _ -> (False, TokenDouble 1, incr'')
                         TokenInt _ -> (False, TokenInt 1, incr'')
                         _ -> usageError argv 
                                "start value required for this type"
                cf' =
                  case incrSign incr' of
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
            in
             (cf', end', start', incr')
  let (start', end', incr') = promote3 (start, end, incr)
  let sequenceStr = 
        map show $ takeWhile (`cf` end') $ 
          iterate (increase incr') start'
  -- Do it
  if gotArg argv ArgNumber
    then numberLines outpad sequenceStr
    else outformat $ outpad sequenceStr
  return ()
