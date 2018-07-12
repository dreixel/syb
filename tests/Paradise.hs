{-# OPTIONS -fglasgow-exts #-}

module Paradise (tests) where

{-

This test runs the infamous PARADISE benchmark,
which is the HELLO WORLD example of generic programming,
i.e., the "increase salary" function is applied to
a typical company just as shown in the boilerplate paper.

-}

import Test.HUnit

import Data.Generics
import CompanyDatatypes
import Data.Ratio

-- Increase salary by percentage
increase :: Rational -> Company -> Company
increase k = everywhere (mkT (incS k))

-- "interesting" code for increase
incS :: Rational -> Salary -> Salary
incS k (S s) = S (s * (1 + k))

tests = increase (1 / 10) genCom ~=? output

output = C [D "Research" (E (P "Laemmel" "Amsterdam") (S 8800)) [PU (E (P "Joost" "Amsterdam") (S 1100)),PU (E (P "Marlow" "Cambridge") (S 2200))],D "Strategy" (E (P "Blair" "London") (S 110000)) []]
