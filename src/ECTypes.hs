{-
  Module      : ECDSA
  Description : The ECDSA implementation in Haskell.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

In this module the Data representation for the Elliptic Curve 
aswell as formatting functions are defined. 

-}
{-# LANGUAGE RecordWildCards #-}

module ECTypes where

import Data.Char (toUpper)
import Numeric (showHex)

-- Mode of operation of the ECDSA.
data Mode
  = Information
  | GenerateKeys
  | Sign
  | Verify

-- The Elliptical Curve representation.
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

-- Show the Curve in a formatted way.
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
    "n: " ++ integerToHexString n ++ "\n" ++ "h: " ++ show h ++ "\n" ++ "}\n"

-- The signing private key 'd'.
type PrivateKey = Integer

-- A 2 dimensional point on the curve.
type Point = (Integer, Integer)

-- The verifying public key 'Q'.
type PublicKey = Point

-- KeyPair
data KeyPair =
  KeyPair
    { d :: PrivateKey
    , q :: PublicKey
    }

-- Show the KeyPair in a formatted way.
instance Show KeyPair where
  show KeyPair {..} =
    "Key {\n" ++
    "d: " ++
    "0x" ++
    pk ++
    "\n" ++
    "Q: " ++
    "0x04" ++
    padPointCoordinate keyLen xpub ++
    padPointCoordinate keyLen ypub ++ "\n" ++ "}\n"
    where
      pk = integerToAlmostHexString d
      (xpub, ypub) = q
      keyLen = length pk

-- The hash of the message to be signed.
type Hash = Integer

-- The ECDSA signature.
data Signature =
  Signature
    { r :: Integer
    , s :: Integer
    }

instance Show Signature where
  show Signature {..} =
    "Signature {\n" ++
    "r: " ++
    integerToHexString r ++
    "\n" ++ "s: " ++ integerToHexString s ++ "\n" ++ "}\n"

-- Convert Integer representation into Hexadecimal string with "0x" prefix.
integerToHexString :: Integer -> String
integerToHexString num = "0x" ++ integerToAlmostHexString num

-- Convert Integer to Hexadecimal without "0x" prefix.
integerToAlmostHexString :: Integer -> String
integerToAlmostHexString num = map toUpper (showHex num "")

{-
   Pad hex string with leading zeros to align with n Bytes
   and return this as a hex string.
-}
padPointCoordinate :: Int -> Integer -> String
padPointCoordinate n integer =
  if length str >= n
    then str
    else padding ++ str
  where
    padding = concat $ replicate (n - length str) "0"
    str = integerToAlmostHexString integer
