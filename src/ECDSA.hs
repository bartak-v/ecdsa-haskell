{-|
  Module      : ECDSA
  Description : The ECDSA implementation in haskell.
  Author:     : Bc. Vít Barták (xbarta47)
  License     : MIT
  Maintainer  : xbarta47@fit.vutbr.cz
  Year        : 2023

This is the ECDSA module in which the Data representation for the Elliptic Curve 
and helper functions are specified.
-}
module ECDSA where

data Curve =
  Curve
    {
    }

data Point =
  Point
    { x :: Integer
    , y :: Integer
    }
  deriving (Show)

type Hash = Integer

type PrivateKey = Integer

type PublicKey = Integer
