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
import System.Random (randomRIO)

-- This function calls the appropriate utility functions based on the Mode. 
processMode :: ECTypes.Mode -> [Char] -> IO ()
processMode mode content =
  case mode of
    ECTypes.Information -> putStr $ show $ ECParser.parseCurve content
    ECTypes.GenerateKeys -> keyGenerator curve
    ECTypes.Sign -> signatureGenerator curve privateKey hash
    ECTypes.Verify -> putStrLn $ "Verification Mode\n" ++ content
  where
    curve = ECParser.parseCurve content
    privateKey = ECParser.parseParam "d:" content
    hash = ECParser.parseParam "Hash:" content

-- Generates ECDSA keys, private key "d" is random nlen bit value.
-- "nlen" is based on the length of "n" prime number order of G in the curve.
-- public key Q is Point on the Curve counted as d*G, where G is the generator point.
keyGenerator :: ECTypes.Curve -> IO ()
keyGenerator curve@ECTypes.Curve {..} = do
  randomNumber <- randomRIO (1, n - 1) -- generate private key in the range (1, n-1)
  let generatorPoint = (x, y) :: ECTypes.Point -- Get the Generator Point out of the curve
      keyPair =
        ECTypes.KeyPair
          { d = randomNumber
          , q = doubleAndAdd curve randomNumber generatorPoint -- Calculate the keypair
          }
  putStr $ ECParser.catCurveKey curve keyPair -- Print it out

-- Generates ECDSA signature of Hash over Curve with PrivateKey.
signatureGenerator ::
     ECTypes.Curve -> ECTypes.PrivateKey -> ECTypes.Hash -> IO ()
signatureGenerator curve@ECTypes.Curve {..} privateKey hash = do
  let truncHash =
        ECParser.integerFromString $
        ECTypes.integerToAlmostHexString $ truncateHash curve hash
  k <- randomRIO (1, n - 1)
  let (r', s') = generateSignature curve privateKey k truncHash -- generate non-null r,s
  if r' == 0 || s' == 0
    then signatureGenerator curve privateKey hash
    else print ECTypes.Signature {r = r', s = s'}

-- Generate signature, until its non-zero (there's a little probability chance that it is zero).
generateSignature ::
     ECTypes.Curve
  -> ECTypes.PrivateKey
  -> Integer
  -> ECTypes.Hash
  -> ECTypes.Point
generateSignature curve@ECTypes.Curve {..} pk k hash = (r, s)
  where
    (x1, _) = doubleAndAdd curve k (x, y) -- generate random point on Curve
    r = x1 `mod` n
    s = ((hash + r * pk) * modularInverse k n) `mod` n

{-
 Truncate Hash to the same length as "n".
 This does not handle the situation that your hash is less in length
 than "n" - use hashes as long or longer than "n".
 Padding and rehashing is not implemented atm - its up to the user
 how long a hash they use.
-}
truncateHash :: ECTypes.Curve -> ECTypes.Hash -> ECTypes.Hash
truncateHash ECTypes.Curve {..} hash =
  ECParser.integerFromString $
  take (length hashStr - (length hashStr - length nStr)) hashStr
  where
    hashStr = ECTypes.integerToAlmostHexString hash
    nStr = ECTypes.integerToAlmostHexString n

-- TODO, dodělat parsování, sign a verify...
-- TODO: vyřešit jak načítat negativní number v pubkey (prostě try and error, hold se to bude počítat dvakrát no)
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

-- Checks wheter a given point is on the Curve.
isPointOnCurve :: ECTypes.Curve -> ECTypes.Point -> Bool
isPointOnCurve ECTypes.Curve {a = a, b = b, p = prime} (x, y) =
  (y * y - x * x * x - a * x - b) `mod` prime == 0

-- Returns negation of a point over Curve.
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

-- Returns the newly calculated point.
calculatePointAdd ::
     ECTypes.Point -> ECTypes.Point -> Integer -> Integer -> ECTypes.Point
calculatePointAdd (x1, y1) (x2, _) prime modulus =
  (x3 `mod` prime, -y3 `mod` prime)
  where
    x3 = modulus * modulus - x1 - x2
    y3 = y1 + modulus * (x3 - x1)

-- TODO check that doublePoint works correctly...
-- Double and add recursive algorithm for scalar point multiplication.
-- Returns scalar*Point (over Curve)
doubleAndAdd :: ECTypes.Curve -> Integer -> ECTypes.Point -> ECTypes.Point
doubleAndAdd curve@ECTypes.Curve {..} scalar point
  | scalar == 0 = (0, 0) -- TODO tohle se musí přepsat, nebo se to někde vejš zachytit, add infinity point
  | scalar == 1 = point
  | scalar `mod` 2 == 1 =
    addPoints curve point $ doubleAndAdd curve (scalar - 1) point -- addition when scalar is odd
  | otherwise = doubleAndAdd curve (div scalar 2) $ doublePoint curve point -- doubling when scalar is even
