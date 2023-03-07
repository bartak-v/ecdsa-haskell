{-|
  Module      : ECDSA
  Description : The ECDSA implementation in haskell.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

This is the ECDSA module in which the core arithmetic functions over curves
are. And ECDSA modes are dispatched from here, aswell as helper functions. 

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
    ECTypes.Information -> putStr $ show curve
    ECTypes.GenerateKeys -> keyGenerator curve
    ECTypes.Sign -> signatureGenerator curve privateKey hash
    ECTypes.Verify -> verifySignature curve publicKey signature hash
  where
    curve = ECParser.parseCurve content
    privateKey = ECParser.parseParam "d:" content :: ECTypes.PrivateKey
    hash = ECParser.parseParam "Hash:" content
    publicKey = ECParser.parsePubKey content
    signature =
      ECTypes.Signature
        (ECParser.parseParam "r:" content)
        (ECParser.parseParam "s:" content)

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
  putStr $ ECParser.catCurveKey curve keyPair

-- Generates ECDSA signature of Hash over Curve with PrivateKey.
-- This function runs recursively until r and s are non-zero 
-- (there's a little probable chance that they are zero).
signatureGenerator ::
     ECTypes.Curve -> ECTypes.PrivateKey -> ECTypes.Hash -> IO ()
signatureGenerator curve@ECTypes.Curve {..} privateKey hash = do
  let truncHash =
        ECParser.integerFromString $
        ECTypes.integerToAlmostHexString $ truncateHash curve hash
  k <- randomRIO (1, n - 1)
  let (r', s') = generateSignature curve privateKey k truncHash -- generate non-null r,s
  if r' == 0 || s' == 0
    then signatureGenerator curve privateKey hash -- generate again if one or both are zero
    else print ECTypes.Signature {r = r', s = s'}

-- Generate ECDSA signature.
generateSignature ::
     ECTypes.Curve
  -> ECTypes.PrivateKey
  -> Integer
  -> ECTypes.Hash
  -> ECTypes.Point
generateSignature curve@ECTypes.Curve {..} pk k hash = (r, s)
  where
    (x1, _) = doubleAndAdd curve k (x, y) -- generate random point on Curve R=k*G
    r = x1 `mod` n
    s = (modularInverse k n * (hash + r * pk)) `mod` n

-- Load the curve, public key, signature and hash from input and verify the signature. 
verifySignature ::
     ECTypes.Curve
  -> ECTypes.PublicKey
  -> ECTypes.Signature
  -> ECTypes.Hash
  -> IO ()
verifySignature curve@ECTypes.Curve {..} (xpub, ypub) ECTypes.Signature {..} hash = do
  let truncHash = truncateHash curve hash -- truncated hash to (at most) nlen length
  if (r >= 1 && r <= (n - 1)) || (s >= 1 && s <= (n - 1)) -- check if signature is in valid range
    then do
      let u1 = (truncHash * modularInverse s n) `mod` n
          u2 = (r * modularInverse s n) `mod` n
          (xr, yr) =
            addPoints
              curve
              (doubleAndAdd curve u1 (x, y))
              (doubleAndAdd curve u2 (xpub, ypub)) -- R = (xR, yR) = u1*G + u2*Q
      if (xr, yr) == (0, 0) -- check if R == Infinity Point
        then do
          putStrLn "False"
        else do
          if r `mod` n == xr `mod` n -- Check the signature
            then do
              putStrLn "True"
            else do
              putStrLn "False"
    else do
      putStrLn "False"

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

-- Add points over a curve.
addPoints :: ECTypes.Curve -> ECTypes.Point -> ECTypes.Point -> ECTypes.Point
addPoints ECTypes.Curve {a = a, p = prime} (x1, y1) (x2, y2)
  | (x1, y1) == (0, 0) = (x2, y2) -- O + P = P 
  | (x2, y2) == (0, 0) = (x1, y1) -- P + O = P
  | x1 == x2 && y1 /= y2 = (0, 0) -- (x1,y1) + (x1,-y1) = O
  | x1 == x2 = calculatePointAdd (x1, y1) (x2, y2) prime lambdaDouble -- Case for point1 == point2 -- double point
  | otherwise = calculatePointAdd (x1, y1) (x2, y2) prime lambdaAdd -- Case for point1 /= point2 -- add points
  where
    lambdaDouble = (3 * x1 * x1 + a) * modularInverse (2 * y1) prime -- Lambda when doubling a point
    lambdaAdd = (y1 - y2) * modularInverse (x1 - x2) prime -- Lambda when adding points

-- Returns the newly calculated point.
calculatePointAdd ::
     ECTypes.Point -> ECTypes.Point -> Integer -> Integer -> ECTypes.Point
calculatePointAdd (x1, y1) (x2, _) prime lambda = (x3, y3)
  where
    x3 = (lambda * lambda - x1 - x2) `mod` prime
    y3 = (lambda * (x1 - x3) - y1) `mod` prime

-- Double and add recursive algorithm for scalar point multiplication.
-- Returns scalar*Point (over Curve).
doubleAndAdd :: ECTypes.Curve -> Integer -> ECTypes.Point -> ECTypes.Point
doubleAndAdd curve@ECTypes.Curve {..} scalar point
  | scalar == 0 = (0, 0) -- (0,0) represents the Infinity Point (O)
  | scalar == 1 = point
  | odd scalar =
    addPoints curve point $ doubleAndAdd curve (scalar - 1) point -- addition when scalar is odd
  | otherwise = doubleAndAdd curve (div scalar 2) $ addPoints curve point point -- doubling when scalar is even
