{-|
  Module      : Main
  Description : Main module of the ECDSA Haskell program.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

This program has been written and tested in GHC version 9.2.5 

This is the Main module of the ECDSA in Haskell program in which 
the command line arguments are parsed and the program is branched
based upon the choice of the user.

For compilation call 'make'.
For usage, call the program with the '--help' flag.
-}
module Main where

import qualified ECDSA (processMode)
import qualified ECTypes(Mode(..))

import System.Environment (getArgs)

help :: String
help =
  "This program is ECDSA implementation in Haskell for 2022/2023 FLP course at BUT FIT.\n \
\Usage: \n\
\   ./flp22-fun [OPTIONS] <FILE>\n\n\
\   FILE:\n\
\    <FILE> can either be text file with Elliptic Curve (EC) information similar to example files in 'test/'.\n\
\    If <FILE> is left empty the program will read STDIN awaiting formatted string\
\ similiar to files in 'test/.'\n\n\
\   OPTIONS:\n\
\     -i ... Info:   Load the EC info from <FILE> to inner representation and print it out to STDOUT.\n\
\     -k ... Keys:   Load the EC info from <FILE> and generate fresh pair of keys (d,Q) to STDOUT.\n\
\     -s ... Sign:   Load the EC info from <FILE> containing private key and message digest (hash) to be signed\n\
\                    and print generated signature (r,s) to STDOUT.\n\
\     -v ... Verify: Load the EC info from <FILE> containing public key, message and signature (r,s)\n\
\                    and print 'True' or 'False' to STDOUT if the signature is correct or not respectively.\n"

{-|
  The main function is branching the program based on the number of 
  command line arguments. 
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    -- One flag argument and STDIN
    [arg] -> do
      content <- getContents
      processAction arg content
    -- One flag argument and input text file
    [arg, file] -> do
      content <- readFile file
      processAction arg content
    -- Zero, three or more arguments
    _ -> putStrLn "Usage: ./flp22-fun [-i | -k | -s | -v | --help] <file>"

{-|
  The 'processAction' function calls the appropriate ECDSA mode 
  based on command line flag argument.
  It takes two arguments 'choice' and 'content' of type 'String'.
-}
processAction :: String -> String -> IO ()
processAction "" "" = putStrLn "No argument or content specified."
processAction choice content =
  case choice of
    "-i" -> ECDSA.processMode ECTypes.Information content
    "-k" -> ECDSA.processMode ECTypes.GenerateKeys content
    "-s" -> ECDSA.processMode ECTypes.Sign content
    "-v" -> ECDSA.processMode ECTypes.Verify content
    "--help" -> putStr help
    _ -> putStrLn "Bad argument provided, use '--help' for more information."
