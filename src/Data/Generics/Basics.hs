-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Basics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell.
-- See <http://www.cs.vu.nl/boilerplate/>. This module provides
-- the 'Data' class with its primitives for generic programming,
-- which is now defined in @Data.Data@. Therefore this module simply
-- re-exports @Data.Data@.
--
-- For more information, please visit the new
-- SYB wiki: <http://www.cs.uu.nl/wiki/bin/view/GenericProgramming/SYB>.
--
-----------------------------------------------------------------------------

module Data.Generics.Basics (
        module Data.Data
  ) where

import Data.Data
