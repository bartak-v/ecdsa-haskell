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

import qualified ECTypes
import qualified ECParser

{-- FUNCTION SECTION --}
-- This function calls the appropriate utility functions based on the Mode. 
processMode :: ECTypes.Mode -> [Char] -> IO ()
processMode mode content =
  case mode of
    ECTypes.Information -> print $ ECParser.parseCurve content
    ECTypes.GenerateKeys -> putStrLn $ "Key Generation Mode\n" ++ content
    ECTypes.Sign -> putStrLn $ "Signature Mode\n" ++ content
    ECTypes.Verify -> putStrLn $ "Verification Mode\n" ++ content
