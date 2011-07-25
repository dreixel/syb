{-# OPTIONS_GHC -cpp                  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Generics.Basics)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. To scrap your boilerplate it
-- is sufficient to import the present module, which simply re-exports all
-- themes of the Data.Generics library.
--
-- For more information, please visit the new
-- SYB wiki: <http://www.cs.uu.nl/wiki/bin/view/GenericProgramming/SYB>.
--
-----------------------------------------------------------------------------

module Data.Generics (

  -- * All Data.Generics modules
  module Data.Data,               -- primitives and instances of the Data class
  module Data.Generics.Aliases,   -- aliases for type case, generic types
  module Data.Generics.Schemes,   -- traversal schemes (everywhere etc.)
  module Data.Generics.Text,      -- generic read and show
  module Data.Generics.Twins,     -- twin traversal, e.g., generic eq
  module Data.Generics.Builders,  -- term builders

#ifdef __GLASGOW_HASKELL__
#ifndef __HADDOCK__
        -- Data types for the sum-of-products type encoding;
        -- included for backwards compatibility; maybe obsolete.
        (:*:)(..), (:+:)(..), Unit(..)
#endif
#endif

 ) where

------------------------------------------------------------------------------

#ifdef __GLASGOW_HASKELL__
#ifndef __HADDOCK__
        -- Data types for the sum-of-products type encoding;
        -- included for backwards compatibility; maybe obsolete.
import GHC.Base ( (:*:)(..), (:+:)(..), Unit(..) )
#endif
#endif

import Data.Data
import Data.Generics.Instances ()
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Generics.Text
import Data.Generics.Twins
import Data.Generics.Builders
