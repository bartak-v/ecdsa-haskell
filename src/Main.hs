{-|
  Module      : Main
  Description : Main module of the ECDSA Haskell program.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

This is the Main module of the ECDSA in Haskell program in which 
the command line arguments are parsed and the program is branched
based upon the choice of the user.

For compilation call 'make'.
For usage, call the program with the '--help' flag.
-}
module Main where

import System.Environment (getArgs)

help :: String
help = "This program is ECDSA implementation in Haskell.\n \
\Usage: \n\
\   ./flp22-fun [OPTIONS] <FILE>\n\
\   FILE:\n\
\    <FILE> can either be text file with Elliptic Curve (EC) information similar to files in 'test/'\n\
\    or if left empty, the program will read STDIN awaiting formatted string similiar to files in 'test/'\n\n\
\   OPTIONS:\n\
\     -i ... Load the EC info from <FILE> to inner representation and print it out to STDOUT.\n\
\     -k ... Load the EC info from <FILE> and print freshly generated pair of keys (d,Q) to STDOUT.\n\
\line3"

{-|
  The main function is branching the program based on the number of 
  command line arguments. 
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    -- | One flag argument and STDIN
    [arg] -> do
      content <- getContents
      runAction arg content
    -- | One flag argument and input text file
    [arg, file] -> do
      content <- readFile file
      runAction arg content
    -- | Zero, three or more arguments
    _ -> putStrLn "Usage: ./flp22-fun [-i | -k | -s | -v | --help] <file>"

{-|
  The 'runAction' function calls the appropriate ECDSA mode 
  based on command line flag argument.
  It takes two arguments 'choice' and 'content' of type 'String'.
-}
runAction :: String -> String -> IO ()
runAction "" "" = putStrLn "No argument or content specified"
runAction choice content =
  case choice of
    "-i" -> putStrLn $ "i" ++ content
    "-k" -> putStrLn $ "i" ++ content
    "-s" -> putStrLn $ "i" ++ content
    "-v" -> putStrLn $ "i" ++ content
    "--help" -> putStrLn help
    _ -> putStrLn "Bad argument provided, use '--help' for more information."
