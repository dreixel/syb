{-# LANGUAGE RankNTypes, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Aliases
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- This module provides a number of declarations for typical generic
-- function types, corresponding type case, and others.
--
-----------------------------------------------------------------------------

module Data.Generics.Aliases (

        -- * Combinators which create generic functions via cast
        --
        -- $castcombinators

        -- ** Transformations
        mkT,
        extT,
        -- ** Queries
        mkQ,
        extQ,
        -- ** Monadic transformations
        mkM,
        extM,
        -- ** MonadPlus transformations
        mkMp,
        extMp,
        -- ** Readers
        mkR,
        extR,
        -- ** Builders
        extB,
        -- ** Other
        ext0,
        -- * Types for generic functions
        -- ** Transformations
        GenericT,
        GenericT'(..),
        -- ** Queries
        GenericQ,
        GenericQ'(..),
        -- ** Monadic transformations
        GenericM,
        GenericM'(..),
        -- ** Readers
        GenericR,
        -- ** Builders
        GenericB,
        -- ** Other
        Generic,
        Generic'(..),

        -- * Ingredients of generic functions
        orElse,

        -- * Function combinators on generic functions
        recoverMp,
        recoverQ,
        choiceMp,
        choiceQ,

        -- * Type extension for unary type constructors
        ext1,
        ext1T,
        ext1M,
        ext1Q,
        ext1R,
        ext1B,

        -- * Type extension for binary type constructors
        ext2,
        ext2T,
        ext2M,
        ext2Q,
        ext2R,
        ext2B

  ) where

#ifdef __HADDOCK__
import Prelude
#endif
import Control.Monad
import Data.Data

------------------------------------------------------------------------------
--
--      Combinators to "make" generic functions
--      We use type-safe cast in a number of ways to make generic functions.
--
------------------------------------------------------------------------------

-- $castcombinators
--
-- Other programming languages sometimes provide an operator @instanceof@ which
-- can check whether an expression is an instance of a given type. This operator
-- allows programmers to implement a function @f :: forall a. a -> a@ which exhibits
-- a different behaviour depending on whether a `Bool` or a `Char` is passed.
-- In Haskell this is not the case: A function with type @forall a. a -> a@
-- can only be the identity function or a function which loops indefinitely
-- or throws an exception. That is, it must implement exactly the same behaviour
-- for any type at which it is used. But sometimes it is very useful to have
-- a function which can accept (almost) any type and exhibit a different behaviour
-- for different types. Haskell provides this functionality with the 'Typeable'
-- typeclass, whose instances can be automatically derived by GHC for almost all
-- types. This typeclass allows the definition of a functon 'cast' which has type
-- @forall a b. (Typeable a, Typeable b) => a -> Maybe b@. The 'cast' function allows
-- to implement a polymorphic function with different behaviour at different types:
--
-- >>> cast True :: Maybe Bool
-- Just True
--
-- >>> cast True :: Maybe Int
-- Nothing
--
-- This section provides combinators which make use of 'cast' internally to
-- provide various polymorphic functions with type-specific behaviour.


-- | Extend the identity function with a type-specific transformation.
-- The function created by @mkT ext@ behaves like the identity function on all
-- arguments which cannot be cast to type @b@, and like the function @ext@ otherwise.
-- The name 'mkT' is short for "make transformation".
--
-- === __Examples__
--
-- >>> mkT not True
-- False
--
-- >>> mkT not 'a'
-- 'a'
--
-- @since 0.1.0.0
mkT :: ( Typeable a
       , Typeable b
       )
    => (b -> b)
    -- ^ The type-specific transformation
    -> a
    -- ^ The argument we try to cast to type @b@
    -> a
mkT = extT id


-- | The function created by @mkQ def f@ returns the default result
-- @def@ if its argument cannot be cast to type @b@, otherwise it returns
-- the result of applying @f@ to its argument.
-- The name 'mkQ' is short for "make query".
--
-- === __Examples__
--
-- >>> mkQ "default" (show :: Bool -> String) True
-- "True"
--
-- >>> mkQ "default" (show :: Bool -> String) ()
-- "default"
--
-- @since 0.1.0.0
mkQ :: ( Typeable a
       , Typeable b
       )
    => r
    -- ^ The default result
    -> (b -> r)
    -- ^ The transformation to apply if the cast is successful
    -> a
    -- ^ The argument we try to cast to type @b@
    -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r


-- | Extend the default monadic action @pure :: Monad m => a -> m a@ by a type-specific
-- monadic action. The function created by @mkM act@ behaves like 'pure' if its
-- argument cannot be cast to type @b@, and like the monadic action @act@ otherwise.
-- The name 'mkM' is short for "make monadic transformation".
--
-- === __Examples__
--
-- >>> mkM (\x -> [x, not x]) True
-- [True,False]
--
-- >>> mkM (\x -> [x, not x]) (5 :: Int)
-- [5]
--
-- @since 0.1.0.0
mkM :: ( Monad m
       , Typeable a
       , Typeable b
       )
    => (b -> m b)
    -- ^ The type-specific monadic transformation
    -> a
    -- ^ The argument we try to cast to type @b@
    -> m a
mkM = extM return

-- | Extend the default 'MonadPlus' action @const mzero@ by a type-specific 'MonadPlus'
-- action. The function created by @mkMp act@ behaves like @const mzero@ if its argument
-- cannot be cast to type @b@, and like the monadic action @act@ otherwise.
-- The name 'mkMp' is short for "make MonadPlus transformation".
--
-- === __Examples__
--
-- >>> mkMp (\x -> Just (not x)) True
-- Just False
--
-- >>> mkMp (\x -> Just (not x)) 'a'
-- Nothing
--
-- @since 0.1.0.0
mkMp :: ( MonadPlus m
        , Typeable a
        , Typeable b
        )
     => (b -> m b)
     -- ^ The type-specific MonadPlus action
     -> a
     -- ^ The argument we try to cast to type @b@
     -> m a
mkMp = extM (const mzero)


-- | Make a generic reader from a type-specific case.
-- The function created by @mkR f@ behaves like the reader @f@ if an expression
-- of type @a@ can be cast to type @b@, and like the expression @mzero@ otherwise.
-- The name 'mkR' is short for "make reader".
--
-- === __Examples__
--
-- >>> mkR (Just True) :: Maybe Bool
-- Just True
--
-- >>> mkR (Just True) :: Maybe Int
-- Nothing
--
-- @since 0.1.0.0
mkR :: ( MonadPlus m
       , Typeable a
       , Typeable b
       )
    => m b
    -- ^ The type-specific reader
    -> m a
mkR f = mzero `extR` f


-- | Flexible type extension
--
-- === __Examples__
--
-- >>> ext0 [1 :: Int, 2, 3] [True, False] :: [Int]
-- [1,2,3]
--
-- >>> ext0 [1 :: Int, 2, 3] [4 :: Int, 5, 6] :: [Int]
-- [4,5,6]
--
-- @since 0.1.0.0
ext0 :: (Typeable a, Typeable b) => c a -> c b -> c a
ext0 def ext = maybe def id (gcast ext)


-- | Extend a generic transformation by a type-specific transformation.
-- The function created by @extT def ext@ behaves like the generic transformation
-- @def@ if its argument cannot be cast to the type @b@, and like the type-specific
-- transformation @ext@ otherwise.
-- The name 'extT' is short for "extend transformation".
--
-- === __Examples__
--
-- >>> extT id not True
-- False
--
-- >>> extT id not 'a'
-- 'a'
--
-- @since 0.1.0.0
extT :: ( Typeable a
        , Typeable b
        )
     => (a -> a)
     -- ^ The transformation we want to extend
     -> (b -> b)
     -- ^ The type-specific transformation
     -> a
     -- ^ The argument we try to cast to type @b@
     -> a
extT def ext = unT ((T def) `ext0` (T ext))


-- | Extend a generic query by a type-specific query. The function created by @extQ def ext@ behaves
-- like the generic query @def@ if its argument cannot be cast to the type @b@, and like the type-specific
-- query @ext@ otherwise.
-- The name 'extQ' is short for "extend query".
--
-- === __Examples__
--
-- >>> extQ (const True) not True
-- False
--
-- >>> extQ (const True) not 'a'
-- True
--
-- @since 0.1.0.0
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> r)
     -- ^ The query we want to extend
     -> (b -> r)
     -- ^ The type-specific query
     -> a
     -- ^ The argument we try to cast to type @b@
     -> r
extQ f g a = maybe (f a) g (cast a)


-- | Extend a generic monadic transformation by a type-specific case.
-- The function created by @extM def ext@ behaves like the monadic transformation
-- @def@ if its argument cannot be cast to type @b@, and like the monadic transformation
-- @ext@ otherwise.
-- The name 'extM' is short for "extend monadic transformation".
--
-- === __Examples__
--
-- >>> extM (\x -> [x,x])(\x -> [not x, x]) True
-- [False,True]
--
-- >>> extM (\x -> [x,x])(\x -> [not x, x]) (5 :: Int)
-- [5,5]
--
-- @since 0.1.0.0
extM :: ( Monad m
        , Typeable a
        , Typeable b
        )
     => (a -> m a)
     -- ^ The monadic transformation we want to extend
     -> (b -> m b)
     -- ^ The type-specific monadic transformation
     -> a
     -- ^ The argument we try to cast to type @b@
     -> m a
extM def ext = unM ((M def) `ext0` (M ext))


-- | Extend a generic MonadPlus transformation by a type-specific case.
-- The function created by @extMp def ext@ behaves like 'MonadPlus' transformation @def@
-- if its argument cannot be cast to type @b@, and like the transformation @ext@ otherwise.
-- Note that 'extMp' behaves exactly like 'extM'.
-- The name 'extMp' is short for "extend MonadPlus transformation".
--
-- === __Examples__
--
-- >>> extMp (\x -> [x,x])(\x -> [not x, x]) True
-- [False,True]
--
-- >>> extMp (\x -> [x,x])(\x -> [not x, x]) (5 :: Int)
-- [5,5]
--
-- @since 0.1.0.0
extMp :: ( MonadPlus m
         , Typeable a
         , Typeable b
         )
      => (a -> m a)
      -- ^ The 'MonadPlus' transformation we want to extend
      -> (b -> m b)
      -- ^ The type-specific 'MonadPlus' transformation
      -> a
      -- ^ The argument we try to cast to type @b@
      -> m a
extMp = extM


-- | Extend a generic builder by a type-specific case.
-- The builder created by @extB def ext@ returns @def@ if @ext@ cannot be cast
-- to type @a@, and like @ext@ otherwise.
-- The name 'extB' is short for "extend builder".
--
-- === __Examples__
--
-- >>> extB True 'a'
-- True
--
-- >>> extB True False
-- False
--
-- @since 0.1.0.0
extB :: ( Typeable a
        , Typeable b
        )
     => a
     -- ^ The default result
     -> b
     -- ^ The argument we try to cast to type @a@
     -> a
extB a = maybe a id . cast


-- | Extend a generic reader by a type-specific case.
-- The reader created by @extR def ext@ behaves like the reader @def@
-- if expressions of type @b@ cannot be cast to type @a@, and like the
-- reader @ext@ otherwise.
-- The name 'extR' is short for "extend reader".
--
-- === __Examples__
--
-- >>> extR (Just True) (Just 'a')
-- Just True
--
-- >>> extR (Just True) (Just False)
-- Just False
--
-- @since 0.1.0.0
extR :: ( Monad m
        , Typeable a
        , Typeable b
        )
     => m a
     -- ^ The generic reader we want to extend
     -> m b
     -- ^ The type-specific reader
     -> m a
extR def ext = unR ((R def) `ext0` (R ext))



------------------------------------------------------------------------------
--
--      Types for generic functions
--
------------------------------------------------------------------------------


-- | Generic transformations,
--   i.e., take an \"a\" and return an \"a\"
--
-- @since 0.1.0.0
type GenericT = forall a. Data a => a -> a

-- | The type synonym `GenericT` has a polymorphic type, and can therefore not
--   appear in places where monomorphic types are expected, for example in a list.
--   The newtype `GenericT'` wraps `GenericT` in a newtype to lift this restriction.
--
-- @since 0.1.0.0
newtype GenericT' = GT { unGT :: GenericT }

-- | Generic queries of type \"r\",
--   i.e., take any \"a\" and return an \"r\"
--
-- @since 0.1.0.0
type GenericQ r = forall a. Data a => a -> r

-- | The type synonym `GenericQ` has a polymorphic type, and can therefore not
--   appear in places where monomorphic types are expected, for example in a list.
--   The newtype `GenericQ'` wraps `GenericQ` in a newtype to lift this restriction.
--
-- @since 0.1.0.0
newtype GenericQ' r = GQ { unGQ :: GenericQ r }

-- | Generic monadic transformations,
--   i.e., take an \"a\" and compute an \"a\"
--
-- @since 0.1.0.0
type GenericM m = forall a. Data a => a -> m a

-- | The type synonym `GenericM` has a polymorphic type, and can therefore not
--   appear in places where monomorphic types are expected, for example in a list.
--   The newtype `GenericM'` wraps `GenericM` in a newtype to lift this restriction.
--
-- @since 0.1.0.0
newtype GenericM' m = GM { unGM :: GenericM m }

-- | Generic builders
--   i.e., produce an \"a\".
--
-- @since 0.1.0.0
type GenericB = forall a. Data a => a


-- | Generic readers, say monadic builders,
--   i.e., produce an \"a\" with the help of a monad \"m\".
--
-- @since 0.1.0.0
type GenericR m = forall a. Data a => m a


-- | The general scheme underlying generic functions
--   assumed by gfoldl; there are isomorphisms such as
--   GenericT = Generic T.
--
-- @since 0.1.0.0
type Generic c = forall a. Data a => a -> c a


-- | The type synonym `Generic` has a polymorphic type, and can therefore not
--   appear in places where monomorphic types are expected, for example in a list.
--   The data type `Generic'` wraps `Generic` in a data type to lift this restriction.
--
-- @since 0.1.0.0
data Generic' c = Generic' { unGeneric' :: Generic c }

------------------------------------------------------------------------------
--
-- Ingredients of generic functions
--
------------------------------------------------------------------------------

-- | Left-biased choice on maybes
--
-- === __Examples__
--
-- >>> orElse Nothing Nothing
-- Nothing
--
-- >>> orElse Nothing (Just 'a')
-- Just 'a'
--
-- >>> orElse (Just 'a') Nothing
-- Just 'a'
--
-- >>> orElse (Just 'a') (Just 'b')
-- Just 'a'
--
-- @since 0.1.0.0
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y


------------------------------------------------------------------------------
--
-- Function combinators on generic functions
--
------------------------------------------------------------------------------
{-

The following variations take "orElse" to the function
level. Furthermore, we generalise from "Maybe" to any
"MonadPlus". This makes sense for monadic transformations and
queries. We say that the resulting combinators modell choice. We also
provide a prime example of choice, that is, recovery from failure. In
the case of transformations, we recover via return whereas for
queries a given constant is returned.

-}

-- | Choice for monadic transformations
--
-- @since 0.1.0.0
choiceMp :: MonadPlus m => GenericM m -> GenericM m -> GenericM m
choiceMp f g x = f x `mplus` g x


-- | Choice for monadic queries
--
-- @since 0.1.0.0
choiceQ :: MonadPlus m => GenericQ (m r) -> GenericQ (m r) -> GenericQ (m r)
choiceQ f g x = f x `mplus` g x


-- | Recover from the failure of monadic transformation by identity
--
-- @since 0.1.0.0
recoverMp :: MonadPlus m => GenericM m -> GenericM m
recoverMp f = f `choiceMp` return


-- | Recover from the failure of monadic query by a constant
--
-- @since 0.1.0.0
recoverQ :: MonadPlus m => r -> GenericQ (m r) -> GenericQ (m r)
recoverQ r f = f `choiceQ` const (return r)



------------------------------------------------------------------------------
--      Type extension for unary type constructors
------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#define Typeable2 Typeable
#endif

-- | Flexible type extension
--
-- @since 0.3
ext1 :: (Data a, Typeable1 t)
     => c a
     -> (forall d. Data d => c (t d))
     -> c a
ext1 def ext = maybe def id (dataCast1 ext)


-- | Type extension of transformations for unary type constructors
--
-- @since 0.1.0.0
ext1T :: (Data d, Typeable1 t)
      => (forall e. Data e => e -> e)
      -> (forall f. Data f => t f -> t f)
      -> d -> d
ext1T def ext = unT ((T def) `ext1` (T ext))


-- | Type extension of monadic transformations for type constructors
--
-- @since 0.1.0.0
ext1M :: (Monad m, Data d, Typeable1 t)
      => (forall e. Data e => e -> m e)
      -> (forall f. Data f => t f -> m (t f))
      -> d -> m d
ext1M def ext = unM ((M def) `ext1` (M ext))


-- | Type extension of queries for type constructors
--
-- @since 0.1.0.0
ext1Q :: (Data d, Typeable1 t)
      => (d -> q)
      -> (forall e. Data e => t e -> q)
      -> d -> q
ext1Q def ext = unQ ((Q def) `ext1` (Q ext))


-- | Type extension of readers for type constructors
--
-- @since 0.1.0.0
ext1R :: (Monad m, Data d, Typeable1 t)
      => m d
      -> (forall e. Data e => m (t e))
      -> m d
ext1R def ext = unR ((R def) `ext1` (R ext))


-- | Type extension of builders for type constructors
--
-- @since 0.2
ext1B :: (Data a, Typeable1 t)
      => a
      -> (forall b. Data b => (t b))
      -> a
ext1B def ext = unB ((B def) `ext1` (B ext))

------------------------------------------------------------------------------
--      Type extension for binary type constructors
------------------------------------------------------------------------------

-- | Flexible type extension
ext2 :: (Data a, Typeable2 t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)


-- | Type extension of transformations for unary type constructors
--
-- @since 0.3
ext2T :: (Data d, Typeable2 t)
      => (forall e. Data e => e -> e)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> t d1 d2)
      -> d -> d
ext2T def ext = unT ((T def) `ext2` (T ext))


-- | Type extension of monadic transformations for type constructors
--
-- @since 0.3
ext2M :: (Monad m, Data d, Typeable2 t)
      => (forall e. Data e => e -> m e)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> m (t d1 d2))
      -> d -> m d
ext2M def ext = unM ((M def) `ext2` (M ext))


-- | Type extension of queries for type constructors
--
-- @since 0.3
ext2Q :: (Data d, Typeable2 t)
      => (d -> q)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q def ext = unQ ((Q def) `ext2` (Q ext))


-- | Type extension of readers for type constructors
--
-- @since 0.3
ext2R :: (Monad m, Data d, Typeable2 t)
      => m d
      -> (forall d1 d2. (Data d1, Data d2) => m (t d1 d2))
      -> m d
ext2R def ext = unR ((R def) `ext2` (R ext))


-- | Type extension of builders for type constructors
--
-- @since 0.3
ext2B :: (Data a, Typeable2 t)
      => a
      -> (forall d1 d2. (Data d1, Data d2) => (t d1 d2))
      -> a
ext2B def ext = unB ((B def) `ext2` (B ext))

------------------------------------------------------------------------------
--
--      Type constructors for type-level lambdas
--
------------------------------------------------------------------------------


-- | The type constructor for transformations
newtype T x = T { unT :: x -> x }

-- | The type constructor for transformations
newtype M m x = M { unM :: x -> m x }

-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | The type constructor for readers
newtype R m x = R { unR :: m x }

-- | The type constructor for builders
newtype B x = B {unB :: x}
