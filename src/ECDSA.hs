{-|
  Module      : ECDSA
  Description : The ECDSA implementation in haskell.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

This is the ECDSA module in which the Data representation for the Elliptic Curve 
and the core computing functions are specified, aswell as helper functions. 

The module is fully utilizing the "infinite" Integer type for representing parts
of the ECDSA standard and making calculations.
-}
{-# LANGUAGE RecordWildCards #-}

module ECDSA where

import Data.Char (isDigit, toUpper)
import Data.List (isInfixOf, isPrefixOf)
import Numeric (showHex)

{-- TYPES SECTION --}
-- Mode of operation of the ECDSA
data Mode
  = Information
  | GenerateKeys
  | Sign
  | Verify

data ECDSA =
  ECDSA
    { curve :: Curve
    , pk :: PrivateKey
    , pubk :: PublicKey
    , hash :: Hash
    , signature :: Signature
    }

-- The Elliptical Curve representation
data Curve =
  Curve
    { p :: Integer
    , a :: Integer
    , b :: Integer
    -- x,y are coordinates of point G on the curve
    , x :: Integer
    , y :: Integer
    , n :: Integer
    , h :: Integer
    }

instance Show Curve where
  show Curve {..} =
    "Curve {\n" ++
    "p: " ++
    integerToHexString p ++
    "\n" ++
    "a: " ++
    show a ++
    "\n" ++
    "b: " ++
    show b ++
    "\n" ++
    "g: Point {" ++
    "\n" ++
    "x: " ++
    integerToHexString x ++
    "\n" ++
    "y: " ++
    integerToHexString y ++
    "\n" ++
    "}\n" ++
    "n: " ++ integerToHexString n ++ "\n" ++ "h: " ++ show h ++ "\n" ++ "}"

-- The hash of the message to be signed
type Hash = Integer

-- The signature
data Signature =
  Signature
    { r :: Integer
    , s :: Integer
    }

-- The signing private key 'd'
type PrivateKey = Integer

-- The verifying public key 'Q'
type PublicKey = Integer

-- TODO remake this to be case insensitive, test if i need both vars
-- Prefixes of the ECDSA parameters to be extracted out of the input file / string.
ecdsaParameters :: [[Char]]
ecdsaParameters =
  ["p:", "a:", "b:", "x:", "y:", "n:", "h:", "d:", "Q:", "Hash:", "r:", "s:"]

-- | TODO: do i even need this?
-- Filtered input keywords that are present in the input file / string.
ecdsaInputKeywords :: [[Char]]
ecdsaInputKeywords =
  ["Curve", "Key", "Point", "g:", "Signature", "{", "}"] ++ ecdsaParameters

{-- FUNCTION SECTION --}
-- This function calls the appropriate utility functions based on the Mode. 
processMode :: Mode -> [Char] -> IO ()
processMode mode content =
  case mode of
    Information -> print $ parseCurve content
    GenerateKeys -> putStrLn $ "Key Generation Mode\n" ++ content
    Sign -> putStrLn $ "Signature Mode\n" ++ content
    Verify -> putStrLn $ "Verification Mode\n" ++ content

-- TODO: create a parsing function that checks, if the
-- input file is in correct format -> contains only allowed keywords
-- Parse and carve out the whole ECDSA Curve out of an input string.
parseCurve :: String -> Curve
parseCurve str =
  Curve
    { p = parseParam "p:" str
    , a = parseParam "a:" str
    , b = parseParam "b:" str
    , x = parseParam "x:" str
    , y = parseParam "y:" str
    , n = parseParam "n:" str
    , h = parseParam "h:" str
    }

{-
This function takes String representing decimal or hex 
 number such as "1234", "0xFFAB" or "FFAB" 
 and converts it to Integer to be further used.
-}
integerFromString :: String -> Integer
integerFromString str
  | all isDigit str || "0x" `isPrefixOf` str = read str
  | otherwise = read $ "0x" ++ str

-- TODO testing: print $ ECDSA.Point (ECDSA.integerFromHexString ("0xFFB"::String)) (ECDSA.integerFromHexString ("FFB"::String))
-- Inverse function to the integerFromHexString
integerToHexString :: Integer -> String
integerToHexString num = "0x" ++ map toUpper (showHex num "")

-- TODO: maybe handle the [] -> case better - do validation of the whole loaded input anyways
-- Extracts specified ECDSA Curve related parameter from the loaded string. (p: etc.)
extractCurveParameter :: String -> [String] -> String -> String
extractCurveParameter wantedParameter keywords rawInputString =
  let filteredLines =
        filter (wantedParameter `isInfixOf`) $ lines rawInputString
   in case filteredLines of
        [] -> " "
        (line:_) -> getTrimmmedParameterValue keywords line

-- Returns trimmed parameter - the value of a parameter ("p: 0xFFFFFFFFFF" -> "0xFFFFFFFFFF") from a line
-- of the input string
getTrimmmedParameterValue :: [String] -> String -> String
getTrimmmedParameterValue parameters line =
  unwords $ filter (`notElem` parameters) $ words line

-- TODO: maybe do this in Parser.hs?
-- Returns extracted ECDSA parameter converted to hex from input string / file.
parseParam :: String -> String -> Integer
parseParam parameter input =
  integerFromString $ extractCurveParameter parameter ecdsaParameters input
