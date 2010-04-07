
module Main where

import Test.HUnit

import qualified Datatype
import qualified FoldTree
import qualified GetC
import qualified GMapQAssoc
import qualified GRead
import qualified GShow
import qualified GShow2
import qualified HList
import qualified HOPat
import qualified Labels
import qualified Newtype
import qualified Perm
import qualified Twin
import qualified Typeable
import qualified Typecase1
import qualified Typecase2
import qualified Where
import qualified XML
import qualified Tree
import qualified Strings
import qualified Reify
import qualified Paradise
import qualified GZip
import qualified GEq
import qualified GenUpTo
import qualified FreeNames
import qualified Ext1
import qualified Bits
import qualified Builders

import qualified Encode           -- no tests, should compile
import qualified Ext              -- no tests, should compile
import qualified GRead2           -- no tests, should compile
import qualified LocalQuantors    -- no tests, should compile
import qualified NestedDatatypes  -- no tests, should compile
import qualified Polymatch        -- no tests, should compile


tests =
  "All" ~: [ Datatype.tests
           , FoldTree.tests
           , GetC.tests
           , GMapQAssoc.tests
           , GRead.tests
           , GShow.tests
           , GShow2.tests
           , HList.tests
           , HOPat.tests
           , Labels.tests
           , Newtype.tests
           , Perm.tests
           , Twin.tests
           , Typeable.tests
           , Typecase1.tests
           , Typecase2.tests
           , Where.tests
           , XML.tests
           , Tree.tests
           , Strings.tests
           , Reify.tests
           , Paradise.tests
           , GZip.tests
           , GEq.tests
           , GenUpTo.tests
           , FreeNames.tests
           , Ext1.tests
           , Bits.tests
           , Builders.tests
           ]

main = putStrLn "Running tests for syb..." >> runTestTT tests
