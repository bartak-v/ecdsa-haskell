{-|
  Module      : Main
  Description : Main module of the ECDSA Haskell program.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Stability   : experimental
  Portability : POSIX

This is the Main module in which the decision making based upon command line arguments is programmed.
-}
-- Project: ECDSA in Haskell
-- Author:  Bc. Vít Barták 
-- Login:   xbarta47
-- Year:    2023
module Main where

import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), hGetContents, openFile)

{-|
  The main function is branching the program based on the number of 
  command line arguments. 
-}
main :: IO ()
main = do
  args <- getArgs
  case args
    -- | One flag argument and STDIN
        of
    [arg] -> do
      content <- getContents
      runAction arg content
    -- | One flag argument and file
    [arg, file] -> do
      content <- readFile file
      runAction arg content
    -- | Zero, three or more arguments
    _ -> putStrLn "Usage: ./flp22-fun [-i | -k | -s | -v] <file>"

{-|
  The 'runAction' function calls appropriate ECDSA mode 
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
    _ -> putStrLn "no arg"
