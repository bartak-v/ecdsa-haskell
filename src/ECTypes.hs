{-|
  Module      : ECDSA
  Description : The ECDSA implementation in haskell.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

In this module the Data representation for the Elliptic Curve 
aswell as helper functions are defined. 

-}
{-# LANGUAGE RecordWildCards #-}

module ECTypes where

import Data.Char (toUpper)
import Numeric (showHex)

-- Mode of operation of the ECDSA
data Mode
  = Information
  | GenerateKeys
  | Sign
  | Verify

data ECDSA =
  ECDSA
    { curve :: Curve
    , key :: Key
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
    "n: " ++ integerToHexString n ++ "\n" ++ "h: " ++ show h ++ "\n" ++ "}\n"

-- A 2 dimensional point on the curve.
type Point = (Integer, Integer) 

-- The signing private key 'd'
type PrivateKey = Integer

-- The verifying public key 'Q'
type PublicKey = Integer

data Key =
  Key
    { d :: PrivateKey
    , q :: PublicKey
    }

instance Show Key where
  show Key {..} =
    "Key {\n" ++
    "d: " ++
    integerToHexString d ++
    "\n" ++ "Q: " ++ integerToHexString q ++ "\n" ++ "}\n"

-- The hash of the message to be signed
type Hash = Integer

-- The signature
data Signature =
  Signature
    { r :: Integer
    , s :: Integer
    }

-- Convert Integer representation into Hexadecimal string with "0x" prefix.
integerToHexString :: Integer -> String
integerToHexString num = "0x" ++ map toUpper (showHex num "")
