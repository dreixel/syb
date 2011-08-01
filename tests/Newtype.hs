{-# OPTIONS -fglasgow-exts #-}

module Newtype (tests) where

-- The type of a newtype should treat the newtype as opaque

import Test.HUnit

import Data.Generics

newtype T = MkT Int deriving( Typeable )

tests = show (typeOf (undefined :: T)) ~=? output

output = "Newtype.T"
