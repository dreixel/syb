{-# OPTIONS -fglasgow-exts #-}

module Builders (tests) where

-- Testing Data.Generics.Builders functionality 

import Test.HUnit

import Data.Data
import Data.Generics.Builders


-- Main function for testing
tests = ( constrs :: [Maybe Int]
        , constrs :: [String]
        , constrs :: [Either Int Float]
        , constrs :: [((), Integer)]
        ) ~=? output

output = ([Nothing,Just 0],["","\NUL"],[Left 0,Right 0.0],[((),0)])