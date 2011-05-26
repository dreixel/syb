{-# OPTIONS_GHC -cpp                  #-}
{-# LANGUAGE Rank2Types               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Triplets
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. The present module 
-- provides support for 3-parameter traversal, which is also 
-- demonstrated with generic zipWith3.
--
-----------------------------------------------------------------------------

module Data.Generics.Triplets (

        -- * Mapping combinators for triplet traversal
        gzipWithT3,
        gzipWithM3,

        -- * A triplet traversal
        gzip3

  ) where


import Prelude hiding (GT)
import Data.Data
import Data.Generics.Aliases
import Data.Generics.Twins


-- | Triplet map for transformation 
gzipWithT3 :: GenericQ (GenericQ (GenericT)) -> GenericQ (GenericQ (GenericT))
gzipWithT3 f x y z =
  case gmapAccumT perkid funs z of
    ([], c) -> c
    _       -> error "gzipWithT3"
  where
    perkid a d = (tail a, unGT (head a) d)
    funs = case gmapAccumQ perkid' funs' y of
             ([], q) -> q
             _       -> error "gzipWithT3"
      where
        perkid' a d = (tail a, unGQ (head a) d)
        funs' = gmapQ (\k -> (GQ (\k' -> GT (f k k')))) x


-- | Triplet map for monadic transformation 
gzipWithM3 :: Monad m
           => GenericQ (GenericQ (GenericM m))
           -> GenericQ (GenericQ (GenericM m))
gzipWithM3 f x y z =
  case gmapAccumM perkid funs z of
    ([], c) -> c
    _       -> error "gzipWithM3"
  where
    perkid a d = (tail a, unGM (head a) d)
    funs = case gmapAccumQ perkid' funs' y of
            ([], q) -> q
            _       -> error "gzipWithM3"
      where
        perkid' a d = (tail a, unGQ (head a) d)
        funs' = gmapQ (\k -> (GQ (\k' -> GM (f k k')))) x


-- | Generic zipWith3
gzip3 :: (forall x. Data x => x -> x -> x -> Maybe x)
      -> (forall x. Data x => x -> x -> x -> Maybe x)
gzip3 f = gzip3' f'
  where
    f' :: GenericQ (GenericQ (GenericM Maybe))
    f' x y z = cast x >>= \x' -> cast y >>= \y' -> f x' y' z
    gzip3' :: GenericQ (GenericQ (GenericM Maybe))
           -> GenericQ (GenericQ (GenericM Maybe))
    gzip3' g x y z = g x y z `orElse`
      if and [toConstr x == toConstr y, toConstr y == toConstr z]
        then gzipWithM3 (gzip3' g) x y z
        else Nothing
