{-|
  Module      : ECDSA
  Description : The ECDSA implementation in haskell.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

This is the ECDSA module in which the Data representation for the Elliptic Curve 
and helper functions are specified. 

The module is fully utilizing the "infinite" Integer type for representing parts
of the ECDSA standard and make calculations.
-}
module ECDSA where


import Text.Parsec
import Text.Parsec.ByteString
import Numeric(showHex)
import Data.ByteString (ByteString, stripPrefix)
import Data.List (isPrefixOf)
import Data.Char (toUpper)


-- | Mode of operation of the ECDSA
data Mode = Information | GenerateKeys | Sign | Verify

-- | The Elliptical Curve representation
data Curve =
  Curve
    { p :: Integer,
      a :: Integer,
      b :: Integer,
      g :: Point,
      n :: Integer,
      h :: Integer
    } 
    deriving (Show)

-- | The Representation of the Point on the Curve
data Point =
  Point
    { x :: Integer
    , y :: Integer
    }
  deriving (Show)

-- | The hash of the message to be signed
type Hash = Integer

-- | The signature
type Signature = Integer

-- | The signing private key 'd'
type PrivateKey = Integer

-- | The verifying public key 'Q'
type PublicKey = Integer


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
integerFromHexString hexString = if "0x" `isPrefixOf` hexString
                then read hexString
                else read ("0x" ++ hexString)
-- TODO testing: print $ ECDSA.Point (ECDSA.integerFromHexString ("0xFFB"::String)) (ECDSA.integerFromHexString ("FFB"::String))

-- | Inverse function to the integerFromHexString
integerToHexString :: Integer -> String
integerToHexString n = "0x" ++ map toUpper ( showHex n "")
