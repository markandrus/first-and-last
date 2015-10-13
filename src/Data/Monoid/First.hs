{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.First
-- Copyright   :  (C) 2015 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- @'First'' n@ is a generalization of the @First@ exported by @Data.Monoid@:
-- whereas @Data.Monoid.First@ returns up to one value, @'First'' n@ returns
-- up to @n@ values.
--
-- @
-- Data.Monoid.First    a ≡
--             'First'' 1 a ≡
--             'First'    a
-- @
--
-- This library also provides an API-compatible type synonym 'First' and
-- function 'getFirst' allowing you to use it as a drop-in replacement.
-------------------------------------------------------------------------------
module Data.Monoid.First
  ( -- * @First@
    First
  , getFirst
    -- * @First'@
  , First'
  , getFirst'
  ) where

import Control.Applicative (Applicative((<*>), pure), Alternative)
import Data.Data (Data)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString(fromString))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Prelude (($), (.), Char, Eq, Foldable(foldr), Functor, Maybe(Just, Nothing), Monoid(mappend, mempty), Ord, Read, Show, Traversable, fromIntegral, head, take)

--------------------------------------------------------------------------------
-- * First
--------------------------------------------------------------------------------

-- | A type isomorphic to @Data.Monoid.First@
type First a = First' 1 a

-- | Get the first value of type @a@, if any.
--
-- >>> getFirst (foldMap pure [])
-- Nothing
--
-- >>> getFirst (foldMap pure [1,2,3,4])
-- Just 1
getFirst :: First a -> Maybe a
getFirst (First' []) = Nothing
getFirst (First' as) = Just . head $ take 1 as

--------------------------------------------------------------------------------
-- * First'
--------------------------------------------------------------------------------

-- | A generalized version of @Data.Monoid.First@
newtype First' (n :: Nat) a = First' { _getFirst' :: [a] } deriving 
  ( Alternative
  , Data
  , Eq
  , Foldable
  , Functor
  , Generic
  , Generic1
  , Ord
  , Read
  , Show
  , Traversable
  )

-- | Get the first @n@ values or fewer of type @a@.
--
-- >>> getFirst' (foldMap pure [1,2,3,4] :: First' 0 Int)
-- []
--
-- >>> getFirst' (foldMap pure [1,2,3,4] :: First' 1 Int)
-- [1]
--
-- >>> getFirst' (foldMap pure [1,2,3,4] :: First' 2 Int)
-- [1,2]
getFirst' :: First' n a -> [a]
getFirst' = _getFirst'

instance KnownNat n => Applicative (First' n) where
  First' l <*> First' r = First' $ l <*> r
  pure a = case natVal (Proxy :: Proxy n) of
    0 -> mempty
    _ -> First' $ pure a

instance KnownNat n => Monoid (First' n a) where
  First' l `mappend` First' r =
    let n = fromIntegral $ natVal (Proxy :: Proxy n)
    in  First' . take n $ l `mappend` r
  mempty = First' mempty

instance KnownNat n => IsString (First' n Char) where
  fromString = foldr (\c a -> pure c `mappend` a) mempty
