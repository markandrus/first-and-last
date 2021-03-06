{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Last
-- Copyright   :  (C) 2015 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- @'Last'' n@ is a generalization of the @Last@ exported by @Data.Monoid@:
-- whereas @Data.Monoid.Last@ returns up to one value, @'Last'' n@ returns
-- up to @n@ values.
--
-- @
-- Data.Monoid.Last    a ≡
--             'Last'' 1 a ≡
--             'Last'    a
-- @
--
-- This library also provides an API-compatible type synonym 'Last' and
-- function 'getLast' allowing you to use it as a drop-in replacement.
-------------------------------------------------------------------------------
module Data.Monoid.Last
  ( -- * @Last@
    Last
  , getLast
    -- * @Last'@
  , Last'
  , getLast'
  ) where

import Control.Applicative (Applicative((<*>), pure), Alternative)
import Data.Data (Data)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString(fromString))
import GHC.Exts (IsList(Item, fromList, toList))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Prelude (($), (.), Char, Eq((==)), Foldable(foldr), Functor, Int, Maybe(Just, Nothing), Monoid(mappend, mempty), Ord, Read, Show, Traversable, fromIntegral, drop, error, head)

-- $setup
-- >>> import Prelude

--------------------------------------------------------------------------------
-- * Last
--------------------------------------------------------------------------------

-- | A type isomorphic to @Data.Monoid.Last@
type Last a = Last' 1 a

-- | Get the last value of type @a@, if any.
--
-- >>> getLast (foldMap pure [])
-- Nothing
--
-- >>> getLast (foldMap pure [1,2,3,4])
-- Just 4
getLast :: Last a -> Maybe a
getLast (getLast' -> []) = Nothing
getLast (getLast' -> as) = Just . head $ takeR 1 as

--------------------------------------------------------------------------------
-- * Last'
--------------------------------------------------------------------------------

-- | A generalized version of @Data.Monoid.Last@
newtype Last' (n :: Nat) a = Last' { _getLast' :: [a] } deriving
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

-- | Get the last @n@ values or fewer of type @a@.
--
-- >>> getLast' (foldMap pure [1,2,3,4] :: Last' 0 Int)
-- []
--
-- >>> getLast' (foldMap pure [1,2,3,4] :: Last' 1 Int)
-- [4]
--
-- >>> getLast' (foldMap pure [1,2,3,4] :: Last' 2 Int)
-- [3,4]
getLast' :: Last' n a -> [a]
getLast' = _getLast'

instance KnownNat n => Applicative (Last' n) where
  Last' l <*> Last' r = Last' $ l <*> r
  pure a =
    let n = natVal (Proxy :: Proxy n)
    in  Last' $ if n == 0 then mempty else pure a

instance KnownNat n => Monoid (Last' n a) where
  Last' l `mappend` Last' r =
    let n = fromIntegral $ natVal (Proxy :: Proxy n)
    in  Last' . takeR n $ l `mappend` r
  mempty = Last' mempty

-- https://www.joachim-breitner.de/blog/600-On_taking_the_last_n_elements_of_a_list
takeR :: Int -> [a] -> [a]
takeR n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys
    go (_:_) [] = error "impossible!"

instance KnownNat n => IsList (Last' n a) where
  type Item (Last' n a) = a
  fromList = foldr (\c a -> pure c `mappend` a) mempty
  toList = getLast'

instance KnownNat n => IsString (Last' n Char) where
  fromString = fromList
