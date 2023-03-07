{-|
  Module      : ECParser
  Description : Parser functions for Elliptic Curve DSA.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

This module functions as a Parser for the custom Elliptic Curve input format.
-}
module ECParser where

import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import qualified ECTypes

-- Prefixes of the ECDSA parameters to be extracted out of the input file / string.
ecdsaParameters :: [[Char]]
ecdsaParameters =
  ["p:", "a:", "b:", "x:", "y:", "n:", "h:", "d:", "Q:", "Hash:", "r:", "s:"]

-- Parse and carve out the whole ECDSA Curve out of an input string.
parseCurve :: String -> ECTypes.Curve
parseCurve str =
  ECTypes.Curve
    { ECTypes.p = ECParser.parseParam "p:" str
    , ECTypes.a = parseParam "a:" str
    , ECTypes.b = parseParam "b:" str
    , ECTypes.x = parseParam "x:" str
    , ECTypes.y = parseParam "y:" str
    , ECTypes.n = parseParam "n:" str
    , ECTypes.h = parseParam "h:" str
    }

-- Return formatted Curve with KeyPair as a String.
catCurveKey :: ECTypes.Curve -> ECTypes.KeyPair -> String
catCurveKey curve keyPair = show curve ++ show keyPair

{-
This function takes String representing decimal or hex 
 number such as "1234", "0xFFAB" or "FFAB" 
 and converts it to Integer to be further used.
-}
integerFromString :: String -> Integer
integerFromString str
  | all isDigit str || "0x" `isPrefixOf` str = read str
  | otherwise = read $ "0x" ++ str

-- Extracts specified ECDSA Curve related parameter from the loaded string. (p: etc.)
extractCurveParameter :: String -> [String] -> String -> String
extractCurveParameter wantedParameter keywords rawInputString =
  let filteredLines =
        filter (wantedParameter `isInfixOf`) $ lines rawInputString
   in case filteredLines of
        [] -> " "
        (line:_) -> getTrimmmedParameterValue keywords line

-- Returns trimmed parameter - the value of a parameter ("p: 0xFFFFFFFFFF" -> "0xFFFFFFFFFF") 
-- from a line of the input string
getTrimmmedParameterValue :: [String] -> String -> String
getTrimmmedParameterValue parameters line =
  unwords $ filter (`notElem` parameters) $ words line

-- Returns extracted ECDSA parameter converted to integer from input string / file.
parseParam :: String -> String -> Integer
parseParam parameter input =
  integerFromString $ extractCurveParameter parameter ecdsaParameters input

-- Returns parsed public key out of the input string.
parsePubKey :: String -> ECTypes.PublicKey
parsePubKey str = (xpub, ypub)
  where
    nlen = length $ ECTypes.integerToAlmostHexString $ parseParam "n:" str
    xpub = integerFromString $ take nlen trimmedPubKeyStr -- Get the x and y coordinate from uncompressed pubkey
    ypub = integerFromString $ take nlen (drop nlen trimmedPubKeyStr)
    trimmedPubKeyStr = drop 4 $ extractCurveParameter "Q:" ecdsaParameters str -- remove 0x04 prefix from PubKey String
