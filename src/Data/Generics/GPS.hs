{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.GPS
-- Copyright   :  ?
-- License     :  BSD-style
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- "Data.Generics" with @GPS@ (Generic Positioning System;-)
--
-- @GPS@ employs 'Map's, to avoid getting lost in 'Data':
--
-- - for each traversed type, build a 'Map' 'TypeKey' 'TypeSet', mapping all
--   substructure types of the given type to their substructure types 
--
-- - traversals are short-circuited when the domain types of their queries 
--   or transformations cannot be found in the current substructure types
--
-- - domains of queries and transformations are computed on construction
--
-- @GPS@ is inspired by /Uniplate/'s @PlateData@ direction finder (@contains@
-- and @DataBox@ are copied from the /Uniplate/ paper), generalised to tackle
-- /SYB/'s more general queries and transformations (instead of oracles telling
-- whether to Stop, Follow, or Find in a search for type b in type a, we 
-- build IntSets of TypeRep keys, both for the domains of traversals and 
-- for substructure types; then several short-circuiting decisions can be 
-- based on fast intersection tests with the same IntSet).
--
-- 'TypeRep' keys combined with 'IntSet's or 'IntMap's can be used to speed up
-- other generic programming problems as well, including typecase and
-- extensible records libraries.
--
-----------------------------------------------------------------------------



module Data.Generics.GPS(
    everything, everywhere,
     GenericDomainQ, apQ, asQ, mkQ, extQ, ext1Q,
     -- gmapQ, apQWithMap,
     fromRoot, TypeMap, TypeSet, TypeKey,
     GenericDomainT, apT, mkT, extT,
     -- gmapT
  ) where

import Data.Generics hiding (everything, everywhere, mkQ, extQ, ext1Q, gmapQ, mkT, extT, gmapT, unGQ)
import qualified Data.Generics as DG(mkQ, extQ, ext1Q, gmapQ, mkT, extT, gmapT)

import Prelude hiding (map)
import Data.List hiding ((\\), map)
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.IntSet as IS
import qualified Data.IntMap as Map
import Debug.Trace
import Data.Maybe

type TypeSet = IS.IntSet
type TypeKey = Int
type TypeMap = Map.IntMap
-- type TypeQueryMap r = forall a . Data a => TypeMap (a -> r)
type TypeQueryMap r = TypeMap (GQ r)

data GQ r = forall a . Typeable a  => GQ0 (a -> r) 
          | forall t . Typeable1 t => GQ1 (forall d . Data d => t d -> r)
          | GQ2 (GenericQ r)

-- | Variation of 'GenericQ' that provides access to the active domain
--   and default query of the generic query
data GenericDomainQ r = 
  GenericDomainQ { -- queryDomain  :: TypeSet,
                   defaultQuery :: GenericQ r,
                   genericQuery :: GenericQ r,
                   queries      :: TypeQueryMap r }

-- | apply a 'GenericDomainQ'
{-# INLINE apQ #-}
apQ :: GenericDomainQ r -> GenericQ r
-- apQ GenericDomainQ{queryDomain=qd,genericQuery=gq} x = gq x
apQ GenericDomainQ{defaultQuery=dq,queries=qs} x 
  = unGQ $ Map.findWithDefault def key' qs
  where typeRep        = typeOf x
        (tyCon,tyArgs) = splitTyConApp typeRep
        key'           = typeRepKey' typeRep
        key1'          = typeRepKey' (mkTyConApp tyCon (init tyArgs))

        def | null tyArgs = GQ2 dq
            | otherwise   = Map.findWithDefault (GQ2 dq) key1' qs
        unGQ (GQ0 q) = (error "oops" `DG.mkQ` q) x
        unGQ (GQ1 q) = (error "oops" `DG.ext1Q` q) x
        unGQ (GQ2 q) = q x

-- apQWithMap :: TypeMap TypeSet -> GenericDomainQ r -> GenericQ r
-- apQWithMap subMap GenericDomainQ{queryDomain=qd,defaultQuery=dq,genericQuery=gq} x
--   | not $ IS.null $ qd `IS.intersection` getInfo subMap x = gq x
--   | otherwise = dq x

-- | use a generic query as default query for a 'GenericDomainQ'
{-# INLINE asQ #-}
asQ :: GenericQ b -> GenericDomainQ b
asQ def =
  GenericDomainQ{ -- queryDomain  = IS.empty, -- the domain lists only special cases
                  defaultQuery = def,
                  genericQuery = def,
                  queries      = Map.empty 
                }

-- | add a default value to turn a specific query into a 'GenericDomainQ'
{-# INLINE mkQ #-}
mkQ :: forall a r . (Typeable a) => r -> (a -> r) -> GenericDomainQ r
def `mkQ` spec = 
  GenericDomainQ{ -- queryDomain  = IS.singleton specDomainKey,
                  defaultQuery = const def,
                  genericQuery = def `DG.mkQ` spec,
                  queries      = Map.singleton specDomainKey (pq spec) 
                }
  where specDomainKey = getDomainKey spec

        pq q = GQ0 $ q

-- | extend a 'GenericDomainQ' with a specific query function
{-# INLINE extQ #-}
extQ :: (Typeable a) => GenericDomainQ b -> (a -> b) -> GenericDomainQ b
gen `extQ` spec = 
  gen{ -- queryDomain  = IS.insert specDomainKey (queryDomain gen),
       genericQuery = genericQuery gen `DG.extQ` spec,
       queries      = Map.insert specDomainKey (pq spec) (queries gen) 
     }
  where specDomainKey = getDomainKey spec
        pq q = GQ0 $ q

-- | like 'extQ', but for query functions polymorphic in one type parameter
{-# INLINE ext1Q #-}
ext1Q :: forall t q . Typeable1 t
      => GenericDomainQ q 
      -> (forall d1. (Data d1) => t d1 -> q) 
      -> GenericDomainQ q
gen `ext1Q` spec = 
  gen{ -- queryDomain  = IS.insert specDomainKey1 (queryDomain gen),
       genericQuery = genericQuery gen `DG.ext1Q` spec,
       queries      = Map.insert specDomainKey1 (pq spec) (queries gen) 
     }
  where specDomainKey1 = typeRepKey' $ typeOf1 (undefined::t ())
        pq :: (forall d1. (Data d1) => t d1 -> r) -> GQ r
        pq q = GQ1 $ q

-- | Variation of 'GenericT' that provides access to the active domain
--   and default traversal of the generic traversal
data GenericDomainT = 
  GenericDomainT { transDomain  :: TypeSet,
                   defaultTrans :: GenericT,
                   genericTrans :: GenericT }
                   -- does still use nested generic extension with linear search
                   -- instead of selecting matching specific functions from a Map;
                   -- but since that seems to add more overhead than speedup for
                   -- queries, it is probably not worth trying here, either

-- | apply a 'GenericDomainT'
{-# INLINE apT #-}
apT :: GenericDomainT -> GenericT 
apT GenericDomainT{genericTrans=gt} = gt

-- | turn a specific transformation into a 'GenericDomainT'
{-# INLINE mkT #-}
mkT ::  (Typeable b) => (b -> b) -> GenericDomainT
mkT spec = 
  GenericDomainT { transDomain  = IS.singleton (getDomainKey spec),
                   defaultTrans = id,
                   genericTrans = DG.mkT spec }

-- | extend a 'GenericDomainT' with a specific transformation
{-# INLINE extT #-}
extT :: forall a . Data a => GenericDomainT -> (a -> a) -> GenericDomainT
gen `extT` spec =
  gen { transDomain  = IS.singleton (getDomainKey spec),
        genericTrans = genericTrans gen `DG.extT` spec }
{-
gmapQ :: GenericDomainQ r -> GenericDomainQ [r]
gmapQ q@(GenericDomainQ{queryDomain=qd,defaultQuery=dq,genericQuery=gq}) = 
  GenericDomainQ{queryDomain  = IS.empty,    -- can't capture what gmapQ does..
                 defaultQuery = DG.gmapQ dq, -- should this be gq?
                 genericQuery = DG.gmapQ gq}
-}

-- TODO: fold the decision mapping into Map creation?
-- | variation of 'Data.Generics.everything' that uses information
--   about its query domain and the substructure types of the type
--   to be traversed in order to avoid traversing irrelevant substructures.
{-# INLINE everything #-}
everything :: forall a r . Data a => (r -> r -> r) -> GenericDomainQ r -> a -> r
everything k gdq@(GenericDomainQ{queries=qs}) = 
  everythingWithMap ({-# SCC "subMap" #-} maybe Map.empty (Map.map decision) subMap) k gdq
  where subMap = trace ("subMap{"++show (typeOf (undefined::a))++"}") $ fromRoot (undefined::a)
        decision typeSet = not $ IS.null $ domain `IS.intersection` typeSet
        domain = Map.keysSet qs

{-# INLINE everythingWithMap #-}
everythingWithMap :: forall a r . Data a 
                  => TypeMap Bool -> (r -> r -> r) -> GenericDomainQ r -> a -> r
everythingWithMap subDecisionMap k gdq@(GenericDomainQ{defaultQuery=z}) x
  | getInfo subDecisionMap True x 
  = foldl k (apQ gdq x) (DG.gmapQ (everythingWithMap subDecisionMap k gdq) x)
  | otherwise
  = z x

-- | variation of 'Data.Generics.everywhere' that uses information
--   about its query domain and the substructure types of the type
--   to be traversed in order to avoid traversing irrelevant substructures.
{-# INLINE everywhere #-}
everywhere :: forall a . Data a => GenericDomainT -> a -> a
everywhere gdt@(GenericDomainT{transDomain=domain}) = 
  everywhereWithMap (maybe Map.empty (Map.map decision) subMap) gdt
  where subMap = trace ("subMap{"++show (typeOf (undefined::a))++"}") $ fromRoot (undefined::a)
        decision typeSet = not $ IS.null $ domain `IS.intersection` typeSet

{-# INLINE everywhereWithMap #-}
everywhereWithMap :: forall a . Data a 
                  => TypeMap Bool -> GenericDomainT -> a -> a
everywhereWithMap subMap gdt@(GenericDomainT{transDomain=_,defaultTrans=dt,genericTrans=t}) x
  | getInfo subMap True x
  = t (DG.gmapT (everywhereWithMap subMap gdt) x)
  | otherwise
  = dt x
{-
gmapT :: GenericDomainT -> GenericDomainT
gmapT t@(GenericDomainT{transDomain=td,defaultTrans=dt,genericTrans=gt}) = 
  GenericDomainT{transDomain  = IS.empty,    -- can't capture what gmapT does..
                 defaultTrans = DG.gmapT dt, -- should we use gt here?
                 genericTrans = DG.gmapT gt}
-}

getInfo :: forall a typeInfo . Data a => TypeMap typeInfo -> typeInfo -> a -> typeInfo
getInfo subMap def = \x->Map.findWithDefault def (key x) subMap
  -- where def x = error ("missing key: "++show (typeOf x))

-- TODO: what about nested types?
--  - recognizing nested types, let alone those nested types that have
--    and infinite set of substructure types, seems to be tricky
--  - instead, we count nesting of top-level type constructors while 
--    descending into substructure types, and bail out at an arbitrary
--    bound
--
--  - for fromRoot, we could generate a list of TypeMaps instead of a 
--    single TypeMap, but we'd still be in trouble when filling in the
--    map entry for a nested type (no finite representation of the infinite 
--    number of possible substructure types)
--  - we could try to use the actual data value as a bound for the
--    unfolding of substructure types, but fromRootWithBound would 
--    no longer work on undefined, and the bounds would vary for each
--    value, while efficiency depends on computing maps once per type

-- | for a given 'Data' type, build a map from its substructure types
--   to their substructure types.
fromRoot :: forall a . Data a => a -> Maybe (TypeMap TypeSet)
fromRoot root = resMap
  where resMap = fromRoot' 0 resMap IS.empty root Map.empty

fromRoot' :: (Data a) => Int -> Maybe (TypeMap TypeSet) -> TypeSet 
          -> a -> TypeMap TypeSet -> Maybe (TypeMap TypeSet)
fromRoot' n resMap path root map | Map.member rootKey map = Just map
                                 | isNested root path     = if n>9
                                        -- assume nesting is recursive
                                                            then Nothing
                                                            else subs (n+1)
                                 | otherwise              = subs n
  where subs m   = fromRoots m path' (contains root) map'
        path'    = IS.insert rootKey1 path
        map'     = Just $ Map.insert rootKey (Map.keysSet $ fromJust resMap) map1
        map1     = maybe map 
                    (\tr1k->Map.insert (typeRepKey' tr1k) IS.empty map) 
                    (typeOf1Maybe root)
        rootKey  = key root
        rootKey1 = key1 root

fromRoots :: Int -> TypeSet -> [DataBox] -> Maybe (TypeMap TypeSet) 
          -> Maybe (TypeMap TypeSet)
fromRoots n path rs map = foldl' f map rs
  where f map' (DataBox x) = 
          let resMap = maybe Nothing (fromRoot' n resMap path x) map' 
          in  resMap

isNested :: (Typeable a) => a -> IS.IntSet -> Bool
isNested root path      = IS.member (key1 root) path

getDomainKey :: Typeable a => (a->b) -> TypeKey
getDomainKey f = key (getDomain f)
  where getDomain :: (a->b) -> a
        getDomain = undefined

typeOf1Maybe :: (Typeable a) => a -> Maybe TypeRep
typeOf1Maybe x = case splitTyConApp $ typeOf x of
                  (tyCon,tyArgs) | not (null tyArgs) -> Just $ mkTyConApp tyCon (init tyArgs)
                                 | otherwise         -> Nothing

key :: (Typeable a) => a -> Int
key  x = typeRepKey' (typeOf x)

key1 :: (Typeable a) => a -> Int
key1 x = typeRepKey' (mkTyConApp (typeRepTyCon (typeOf x)) [])

typeRepKey' :: TypeRep -> Int
typeRepKey' tr = unsafePerformIO (typeRepKey tr)

data DataBox = forall a . Data a => DataBox a 
contains :: Data a => a -> [DataBox]
contains x = if isAlgType dtyp then concatMap f ctrs else [ ]
  where
  f c = DG.gmapQ DataBox ((fromConstr c) `asTypeOf` x) 
  ctrs = dataTypeConstrs dtyp
  dtyp = dataTypeOf x
