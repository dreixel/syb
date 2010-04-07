{-# OPTIONS -fglasgow-exts #-}

module Typeable (tests) where

import Test.HUnit

import Data.Typeable

newtype Y e = Y { unY :: (e (Y e)) } 

instance Typeable1 e => Typeable (Y e) where
   typeOf _ = mkTyConApp yTc [typeOf1 (undefined :: e ())]

yTc :: TyCon
yTc = mkTyCon "Typeable.Y"

tests = show (typeOf (undefined :: Y [])) ~=? output

output = "Typeable.Y []"
