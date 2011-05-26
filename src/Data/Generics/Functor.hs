{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE PatternGuards              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Functor
-- Copyright   :  ?
-- License     :  BSD-style
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
--   "Data.Generics.Functor" provides 'Data' \/ 'Typeable' based
--   default method implementations for 'Control.Monad.Functor.fmap'
--   and 'Data.Traversable.traverse'
-- 
-- The code is based on exchanges on @generics\@haskell.org@ in July 2008 
-- on implementing type-changing generic maps using "Data.Generic", and
-- on using private types ('X' marks the spots) to mark 'Functor' parameters,
-- distinguishing them from otherwise identical types that generic traversal
-- might act on.
--
-- Claus' original suggestions used unsafeCoerce to mark the parameter types
-- directly in the type of the data to be traversed, which guided the 
-- traversals, but was unsafe in the presence of non-standard Data instances.
--
-- The ideas of implementing the type-changing traversals via gunfold and of
-- marking the spot only in a separate copy, not in the original, both used
-- in the present code, are due to Oleg.
-----------------------------------------------------------------------------

module Data.Generics.Functor (traverseData, fmapData, crushRight) where

import Control.Applicative
import Data.Data

-- "X marks the spot", in this case to distinguish Functor
-- parameters from equivalent types elsewhere; the idea is
-- that Syb generically transports a function to all
-- substructures, but only applies the function to positions
-- marked in the type
data X = X deriving (Data,Typeable)

-- - since gfoldl alone doesn't account for type-changing
--   traversals, we re-construct the result structures using
--   gunfold
-- - each of these traversals consists of one over-general
--   auxiliary traversal (where the types do not precisely
--   capture the intended changes, to allow for recursion
--   over Data), embedded in a more specific wrapper laying 
--   down the intended types at the top level
-- - the wrapper also constructs a partial copy of the 
--   structure to be traversed, with its type modified to
--   mark the positions on which the argument function should
--   act

-- | Default method implementation for 'Data.Traversable.traverse'.

traverseData :: forall f a b t .
  (Applicative f, Typeable1 f, Typeable a, Typeable b,
    Data (t a), Data (t b), Data (t X))
      => (a -> f b) -> t a -> f (t b)
traverseData f = traverseWithMap f (Dyn (undefined::t X))
  where
  traverseWithMap :: forall f' a' b' x y .
    (Applicative f', Typeable1 f', Typeable a', Typeable b', Data x, Data y)
      => (a' -> f' b') -> Dyn -> x -> f' (y)
  traverseWithMap f' (Dyn t) x 
    | typeOf t == typeOf X = maybe (error "traverseData: non-applicable type marked") ($ x) (cast f')
    | otherwise          = rebuildWith (traverseWithMap f') t1 x
    where t1 = Dyn (fromConstr (toConstr x) `asTypeOf` t)

rebuildWith :: forall f x y . (Applicative f, Data x, Data y) 
            => (forall x' y' . (Data x', Data y') => Dyn -> x' -> f y') 
                  -> Dyn -> x -> f y
rebuildWith f (Dyn t) x = case gunfold (k f) (\g -> UnfldStateF2 (pure g) tkids kids) con of
    UnfldStateF2 a [] [] -> a
    _                    -> error "rebuildWith: ?"
  where
    (_,tkids) = dynamize t
    (con,kids) = dynamize x
    k :: forall a b . (Data a)
      => (forall x' y' . (Data x', Data y') => Dyn -> x' -> f y')
            -> UnfldStateF2 f (a -> b) -> UnfldStateF2 f b
    k f' (UnfldStateF2 ca (tkid:tkids') ((Dyn kid):kids')) = UnfldStateF2 (ca <*> (f' tkid kid)) tkids' kids'
    k _ _ = error "rebuildWith: ?"
  

data UnfldStateF2 f a = UnfldStateF2 (f a) [Dyn] [Dyn]

data Dyn = forall a. Data a => Dyn a
data Kids a = Kids { growUp:: [Dyn] }

dynamize :: Data a => a -> (Constr, [Dyn])
dynamize x = (toConstr x, growUp $ gfoldl f (const (Kids [])) x)
  where f (Kids l) a = Kids (l ++ [Dyn a])

{-
gmapt gets the value x to traverse and the template. The template is a
Dyn whose type has the same basic structure as that of x. The following
equation is supposed to hold:
  tt{X:=a} = typeOf x where (Dyn t) = template; tt = typeOf tt
where {X:=a} is a substitution that replaces all occurrences of a singleton
type X with some other suitable type a.
For example, 
   x has the type        [Int]
   template has the type [X]

   x has the type        Tricky Int Int
   template has the type Tricky X Int

   x has the type        Tricky Int Int
   template has the type Tricky X X
Although 'x' is the defined value, template is generally an undefined value.
The trick is to build the template `out of nothing', in a shallow way,
to the extent to enable further traversal. The trick is the 
observation that x and template should share the same data structure, 
or at least the same top-level data constructor.

The following includes an optimization: if typeof template == typeof x,
there is nothing to traverse. Only values that correspond to the mark X
in the template are mapped.
-}

-- | Default method implementation for 'Control.Monad.Functor.fmap'

fmapData :: forall a b c . (Data a, Data b, Data (c a), Data (c b), Data (c X))
         => (a -> b) -> c a -> c b
fmapData f = gmapt f (Dyn (undefined::c X))
  where
  gmapt :: forall d e x y . (Data d, Data e, Data x, Data y) 
        => (d -> e) -> Dyn -> x -> y
  gmapt g trep = maybe (\x -> traverse (trep,x)) ifmarked $ castfn g
   where
    -- ifmarked :: Typeable x => (x -> y) -> (x -> y)
    ifmarked h x | hasmark trep = h x
    ifmarked _ x                = traverse (trep, x)
    -- optimization: t has no mark, there is nothing to map under it

    traverse (Dyn t,x) | typeOf t == typeOf x = 
                maybe (error "traverse1") id $ cast x
    traverse (Dyn t,x) | (tcon,tkids) <- splitTyConApp (typeOf t),
                         (con,kids)   <- splitTyConApp (typeOf x),
                         not (length tkids == length kids &&
                              tcon == con) =
       error $ unwords ["template type", show (typeOf t),
                        "inconsistent with value type", show (typeOf x)]
    traverse (Dyn t, x) = rebuild (dynamize t1) xdyn
      where xdyn@(con,_) = dynamize x
            t1 = fromConstr con `asTypeOf` t

    rebuild (_, tkids) (con, kids) = 
         case gunfold k (\h -> UnfldStateT h tkids kids) con of
            UnfldStateT a [] [] -> a
            _                   -> error "fmapData: ?"
      where
        k (UnfldStateT ca (tkid:tkids') ((Dyn kid):kids')) = 
              UnfldStateT (ca (gmapt f tkid kid)) tkids' kids'
        k _ = error "fmapData: ?"

data UnfldStateT a = UnfldStateT a [Dyn] [Dyn]

castfn :: (Typeable a, Typeable b, Typeable c, Typeable d) 
       => (a -> b) -> Maybe (c -> d)
castfn f = cast f

-- Check whether this type representation corresponds to the elements 
-- (the X things)
hasmark :: Dyn -> Bool
hasmark (Dyn x) = typeOf x == typeOf X


-- The general SYB version of crush in Oleg and Claus style.
crusht :: (Data a, Data x) => (a -> b -> b) -> x -> b -> Dyn -> b
crusht combine val initial trep
  = case cast val of
      -- val has type a, and trep represents a hole (type X)
      Just val_a | hasmark trep -> combine val_a initial
      -- otherwise find the X's
      _                         -> traverse (trep,val)
  where
   traverse (Dyn t,v) | typeOf t == typeOf v -- default case (no X marker)
                      = initial
   -- traverse in parallel with the type representation
   traverse (Dyn t,x) | (tcon,tkids) <- splitTyConApp (typeOf t),
                        (con,kids)   <- splitTyConApp (typeOf x),
                        not (length tkids == length kids &&
                             tcon == con)
        = error $ unwords ["template type", show (typeOf t),
                           "inconsistent with value type", show (typeOf x)]
   traverse (Dyn t,x) = recurse (dynamize t1) xdyn
     where
       xdyn@(con,_kids) = dynamize x
       t1 = fromConstr con `asTypeOf` t
   recurse (_tcon,tkids) (_con,kids) = foldr comb' initial (zip tkids kids)
     where
       comb' (trep',Dyn kid) res = crusht combine kid res trep'

crushRight :: forall a b c.
              (Data a, Data (c a), Data (c X)) =>
              (a -> b -> b) -> c a -> b -> b
crushRight combine cont initial = 
    crusht combine cont initial (Dyn (undefined::c X))
