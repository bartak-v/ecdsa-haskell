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

import qualified ECParser
import qualified ECTypes
import System.Random

-- This function calls the appropriate utility functions based on the Mode. 
processMode :: ECTypes.Mode -> [Char] -> IO ()
processMode mode content =
  case mode of
    ECTypes.Information -> putStr $ show $ ECParser.parseCurve content
    ECTypes.GenerateKeys -> generateKeys content -- printing result: putStr $ ECParser.catCurveKey (ECParser.parseCurve content) (ECParser.parseKey content)
    ECTypes.Sign -> putStrLn $ "Signature Mode\n" ++ content
    ECTypes.Verify -> putStrLn $ "Verification Mode\n" ++ content

-- Get the Curve out of the string TODO maybe delete
generateKeys :: String -> IO ()
generateKeys str = keyGenerator $ ECParser.parseCurve str

-- Generates ECDSA keys, private key "d" is random 256 bit value.
keyGenerator :: ECTypes.Curve -> IO ()
keyGenerator ECTypes.Curve {..} = do
  let lowerBound = 2 ^ ((256 - 1) :: Integer)
      upperBound = 2 ^ (256 :: Integer) - 1
  randomNumber <- randomRIO (lowerBound, upperBound)
  print $ ECTypes.integerToHexString randomNumber

-- {Point arithmetics operations} --
--pointAdd :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
--pointAdd prime p1 p2 = 
-- Processes EUA for two integers, returns greatest common denominator and Bezout coefficients.
extendedEuclideanAlgorithm :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclideanAlgorithm a b =
  case b of
    0 -> (a, 1, 0)
    _ -> (gcd, oldBezoutCoefA, oldBezoutCoefB)
  where
    (quotient, remainder) = a `divMod` b
    (gcd, newBezoutCoefA, newBezoutCoefB) =
      extendedEuclideanAlgorithm b remainder
    oldBezoutCoefA = newBezoutCoefB
    oldBezoutCoefB = newBezoutCoefA - quotient * newBezoutCoefB

-- Modular inverse for Integer and (prime) modulus using Extended Euclidean Algorithm.
modularInverse :: Integer -> Integer -> Integer
modularInverse number modulus
  | finalBezoutCoefA < 0 = finalBezoutCoefA + modulus
  | otherwise = finalBezoutCoefA
  where
    (gcd, finalBezoutCoefA, _) = extendedEuclideanAlgorithm number modulus
