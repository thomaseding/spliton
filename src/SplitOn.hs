module SplitOn (
      main
    ) where


import Control.Monad
import Data.List
import Data.List.Split
import System.Environment
import System.Exit


main :: IO ()
main = do
    args <- getArgs
    let opts = parseArgs args
    when (oHelp opts) helpIO
    case oSplitDelim opts of
        Nothing -> helpIO
        Just splitDelim -> case oJoinDelim opts of
            Nothing -> helpIO
            Just joinDelim -> splitIO splitDelim joinDelim


helpIO :: IO ()
helpIO = do
    putStrLn "TODO: Help display"
    exitSuccess


splitIO :: String -> String -> IO ()
splitIO splitDelim joinDelim = do
    contents <- getContents
    let chunks = splitOn splitDelim contents
        res = intercalate joinDelim chunks
    putStrLn res


data Options = Options {
      oHelp :: Bool
    , oSplitDelim :: Maybe String
    , oJoinDelim :: Maybe String
    }


parseArgs :: [String] -> Options
parseArgs = parseArgs' opts
    where
        opts = Options {
              oHelp = False
            , oSplitDelim = Nothing
            , oJoinDelim = Nothing
            }


parseArgs' :: Options -> [String] -> Options
parseArgs' opts args = case args of
    [] -> opts
    "-h" : _ -> opts{ oHelp = True }
    "--help" : _ -> opts{ oHelp = True }
    "-s" : delim : args' -> parseArgs' opts{ oSplitDelim = Just $ decode delim } args'
    "--split" : delim : args' -> parseArgs' opts{ oSplitDelim = Just $ decode delim } args'
    "-j" : delim : args' -> parseArgs' opts{ oJoinDelim = Just $ decode delim } args'
    "--join" : delim : args' -> parseArgs' opts{ oJoinDelim = Just $ decode delim } args'
    _ -> opts{ oHelp = True }


decode :: String -> String
decode str = case str of
    "" -> ""
    '\\' : c : rest -> unescape c : decode rest
    "\\" -> error "Cannot decode: Bad escape sequence"
    c : rest -> c : decode rest


unescape :: Char -> Char
unescape c = case c of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    'v' -> '\v'
    '\\' -> '\\'
    '\'' -> '\''
    '"' -> '"'
    _ -> '?'


