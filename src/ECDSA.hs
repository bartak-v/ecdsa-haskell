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
-- public key Q is Point on the Curve counted as d*G, where G is the generator point.
keyGenerator :: ECTypes.Curve -> IO ()
keyGenerator curve@ECTypes.Curve {..} = do
  let lowerBound = 2 ^ ((256 - 1) :: Integer)
      upperBound = 2 ^ (256 :: Integer) - 1
  randomNumber <- randomRIO (lowerBound, upperBound)
  let generatorPoint = (x,y) :: ECTypes.Point -- Get the Generator Point out of curve
      keyPair = ECTypes.Key {d = randomNumber, q = doubleAndAdd curve randomNumber generatorPoint } -- Calculate the keypair
  putStr $ ECParser.catCurveKey curve keyPair -- Print it out

-- TODO: vyřešit jak ukládat negativní number (prostě try and error, hold se to bude počítat dvakrát no)

-- {Point arithmetics operations} --
-- Processes EUA for two integers, returns greatest common denominator and Bezout coefficients.
extendedEuclideanAlgorithm :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclideanAlgorithm a b =
  case b of
    0 -> (a, 1, 0)
    _ -> (gcd', oldBezoutCoefA, oldBezoutCoefB)
  where
    (quotient, remainder) = a `divMod` b
    (gcd', newBezoutCoefA, newBezoutCoefB) =
      extendedEuclideanAlgorithm b remainder
    oldBezoutCoefA = newBezoutCoefB
    oldBezoutCoefB = newBezoutCoefA - quotient * newBezoutCoefB

-- Modular inverse for Integer and (prime) modulus using Extended Euclidean Algorithm.
modularInverse :: Integer -> Integer -> Integer
modularInverse number modulus
  | finalBezoutCoefA < 0 = finalBezoutCoefA + modulus
  | otherwise = finalBezoutCoefA
  where
    (_, finalBezoutCoefA, _) = extendedEuclideanAlgorithm number modulus

isPointOnCurve :: ECTypes.Curve -> ECTypes.Point -> Bool
isPointOnCurve ECTypes.Curve {a = a, b = b, p = prime} (x, y) =
  (y * y - x * x * x - a * x - b) `mod` prime == 0

negatePoint :: ECTypes.Curve -> ECTypes.Point -> ECTypes.Point
negatePoint ECTypes.Curve {p = prime} (x1, y1)
  | x1 == 0 && y1 == 0 = error "Can not negate (0,0)."
  | otherwise = (x1, -y1 `mod` prime)

-- Double an EC Point
-- TODO jaktože doublePoint a addPoint dávaj na stejný bod jiný Y? -ono je asi negated
doublePoint :: ECTypes.Curve -> ECTypes.Point -> ECTypes.Point
doublePoint ECTypes.Curve {a = a, p = prime} (xp, yp) = (xr, yr)
  where
    lambda = ((3 * xp * xp + a) * modularInverse (2 * yp) prime) `mod` prime
    xr = (lambda * lambda - 2 * xp) `mod` prime
    yr = (lambda * (xp - xr) - yp) `mod` prime

-- Add points over a curve.
-- TODO pořádně si promyslet https://crypto.stackexchange.com/questions/66288/what-is-the-point-at-infinity-on-secp256k1-and-how-to-calculate-it
-- a zjistit, jestli doubling bodu je to samý jako jeho přidání samo k sobě
-- taky zjistit, jak je to s tím bodem v nekonečnu (tady pro něj házim error)
addPoints :: ECTypes.Curve -> ECTypes.Point -> ECTypes.Point -> ECTypes.Point
addPoints ECTypes.Curve {a = a, p = prime} (x1, y1) (x2, y2)
  | x1 == x2 && y1 /= y2 =
    error " Error: Point in infinity not implemented. point + (-point)" -- TODO: použít nějaký datový typ pro reprezentaci tohohle...
  | x1 == x2 = calculatePointAdd (x1, y1) (x2, y2) prime m1 -- Case for point1 == point2
  | otherwise = calculatePointAdd (x1, y1) (x2, y2) prime m2 -- Case for point1 /= point2
  where
    m1 = (3 * x1 * x1 + a) * modularInverse (2 * y1) prime
    m2 = (y1 - y2) * modularInverse (x1 - x2) prime

-- Returns the new point.
calculatePointAdd ::
     ECTypes.Point -> ECTypes.Point -> Integer -> Integer -> ECTypes.Point
calculatePointAdd (x1, y1) (x2, _) prime modulus =
  (x3 `mod` prime, -y3 `mod` prime)
  where
    x3 = modulus * modulus - x1 - x2
    y3 = y1 + modulus * (x3 - x1)

-- TODO check that doublePoint works correctly...
-- Double and add recursive algorithm for scalar point multiplication.
-- Returns scalar*Point (on Fp)
doubleAndAdd :: ECTypes.Curve -> Integer -> ECTypes.Point -> ECTypes.Point
doubleAndAdd curve@ECTypes.Curve {..} scalar point
  | scalar == 0 = (0, 0) -- TODO tohle se musí přepsat, nebo se to někde vejš zachytit, add infinity point
  | scalar == 1 = point
  | scalar `mod` 2 == 1 =
    addPoints curve point $ doubleAndAdd curve (scalar - 1) point -- addition when scalar is odd
  | otherwise = doubleAndAdd curve (div scalar 2) $ doublePoint curve point -- doubling when scalar is even
