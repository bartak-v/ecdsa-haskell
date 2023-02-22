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
module ECDSA where

import Data.Char (toUpper)
import Data.List (isPrefixOf,isInfixOf)
import Numeric (showHex)

-- | Mode of operation of the ECDSA
data Mode
  = Information
  | GenerateKeys
  | Sign
  | Verify

-- | The Elliptical Curve representation
data Curve =
  Curve
    { p :: Integer
    , a :: Integer
    , b :: Integer
    -- | x,y are coordinates of point G on the curve
    , x :: Integer
    , y :: Integer
    , n :: Integer
    , h :: Integer
    }
  deriving (Show)

-- | The Representation of the Point on the Curve
{-data Point =
  Point
    { x :: Integer
    , y :: Integer
    }
  deriving (Show) -}

-- | The hash of the message to be signed
type Hash = Integer

-- | The signature
data Signature =
  Signature
    { r :: Integer
    , s :: Integer
    }

-- | The signing private key 'd'
type PrivateKey = Integer

-- | The verifying public key 'Q'
type PublicKey = Integer

-- TODO remake this to be case insensitive
-- | ECDSA parameters to be extracted out of the input file / string.
ecdsaParameters :: [[Char]]
ecdsaParameters = ["p:","a:","b:","x:","y:","n:","h:","d:","Q:","Hash:","r:","s:"]

-- | Filtered input keywords that are present in the input file / string.
ecdsaInputKeywords :: [[Char]]
ecdsaInputKeywords = ["Curve","Key","Point","g:","Signature", "{", "}"] ++ ecdsaParameters


-- | This function calls the appropriate utility functions based on the Mode. 
processMode :: Mode -> [Char] -> IO ()
processMode mode content =
  case mode of
    Information -> putStrLn $ "Information Mode\n" ++ content
    GenerateKeys -> putStrLn $ "Key Generation Mode\n" ++ content
    Sign -> putStrLn $ "Signature Mode\n" ++ content
    Verify -> putStrLn $ "Verification Mode\n" ++ content

{-
This function takes String representing hexadecimal number such as "0xFFAB"
or "FFAB" and converts it to Integer to be further used.
-}
integerFromHexString :: String -> Integer
integerFromHexString hexString =
  if "0x" `isPrefixOf` hexString
    then read hexString
    else read ("0x" ++ hexString)

-- TODO testing: print $ ECDSA.Point (ECDSA.integerFromHexString ("0xFFB"::String)) (ECDSA.integerFromHexString ("FFB"::String))
-- | Inverse function to the integerFromHexString
integerToHexString :: Integer -> String
integerToHexString num = "0x" ++ map toUpper (showHex num "")

-- | Extracts specified ECDSA Curve related parameter from the loaded string. (p: etc.)
extractCurveParameter :: String -> String -> [String] -> String
extractCurveParameter rawInputString wantedParameter keywords =
  let filteredLines = filter (wantedParameter `isInfixOf`) $ lines rawInputString
  in case filteredLines of
    [] -> " "
    (line:_) -> getTrimmmedECDSAParameter keywords line

-- | Returns trimmed parameter - the value of a parameter ("p: 0xFFFFFFFFFF" -> "0xFFFFFFFFFF") from a line
-- | of the input string
getTrimmmedECDSAParameter :: [String] -> String -> String
getTrimmmedECDSAParameter ecdsaParameters line = unwords $ filter (`notElem` ecdsaParameters) $ words line