{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Control.Applicative (Applicative((<*>), pure), Alternative((<|>), empty))
import Control.DeepSeq (NFData)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString(fromString))
import GHC.Exts (IsList(Item, fromList, toList))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Prelude (($), (.), Char, Eq((==)), Foldable(foldr), Functor(fmap), Int, Maybe(Just, Nothing), Monoid(mappend, mempty), Ord, Read, Show, Traversable(sequenceA), fromIntegral, drop, error, head)

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
newtype Last' (n :: Nat) a = Last' { _getLast' :: (Int, DList a) } deriving
  ( Eq
  , Foldable
  , Functor
  , Generic
  , Generic1
  , NFData
  , Ord
  , Read
  , Show
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
getLast' (Last' (n, as)) = takeR n $ DList.toList as

instance KnownNat n => Alternative (Last' n) where
  Last' (n, l) <|> Last' (_, r) = Last' (n, l <|> r)
  empty = mempty

instance KnownNat n => Applicative (Last' n) where
  Last' (n, l) <*> Last' (_, r) = Last' (n, l <*> r)
  pure a =
    let n = natVal (Proxy :: Proxy n)
    in  Last' (fromIntegral n, if n == 0 then mempty else pure a)

instance KnownNat n => Monoid (Last' n a) where
  Last' (n, l) `mappend` Last' (_, r) = Last' (n, l `mappend` r)
  mempty =
    let n = natVal (Proxy :: Proxy n)
    in  Last' (fromIntegral n, mempty)

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

instance Traversable (Last' n) where
  sequenceA (Last' (n, as)) = fmap (\as' -> Last' (n, fromList as')) . sequenceA $ toList as
