{-# OPTIONS -fglasgow-exts #-}

-- These are simple tests to observe (data)type representations.
module Datatype (tests) where

import Test.HUnit

import Data.Tree
import Data.Generics

-- A simple polymorphic datatype
data Data a =>
     MyDataType a = MyDataType a
                  deriving (Typeable, Data)


-- Some terms and corresponding type representations
myTerm     = undefined :: MyDataType Int
myTypeRep  = typeOf myTerm            -- type representation in Typeable
myTyCon    = typeRepTyCon myTypeRep   -- type constructor via Typeable
myDataType = dataTypeOf myTerm        -- datatype representation in Data
myString1  = tyConString myTyCon      -- type constructor via Typeable
myString2  = dataTypeName myDataType  -- type constructor via Data

-- Main function for testing
tests =  show ( myTypeRep
            , ( myDataType
            , ( tyconModule myString1
            , ( tyconUQname myString1
            , ( tyconModule myString2
            , ( tyconUQname myString2
            ))))))
       ~=? output

output = "(Datatype.MyDataType Int,(DataType {tycon = \"Datatype.MyDataType\", datarep = AlgRep [MyDataType]},(\"Datatype\",(\"MyDataType\",(\"Datatype\",\"MyDataType\")))))"