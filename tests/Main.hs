
module Main where

import Test.HUnit
import System.Exit

import qualified Bits
import qualified Builders
import qualified Datatype
import qualified Ext1
import qualified Ext2
import qualified FoldTree
import qualified FreeNames
import qualified GEq
import qualified GMapQAssoc
import qualified GRead
import qualified GShow
import qualified GShow2
import qualified GZip
import qualified GenUpTo
import qualified GetC
import qualified HList
import qualified HOPat
import qualified Labels
import qualified Newtype
import qualified Paradise
import qualified Perm
import qualified Reify
import qualified Strings
import qualified Tree
import qualified Twin
import qualified Typecase1
import qualified Typecase2
import qualified Where
import qualified XML

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
           , Ext2.tests
           , Bits.tests
           , Builders.tests
           ]

main = do
         putStrLn "Running tests for syb..."
         counts <- runTestTT tests
         if (failures counts > 0)
           then exitFailure
             else exitSuccess
