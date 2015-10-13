{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.First
-- Copyright   :  (C) 2015 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Golden where

import Data.Monoid.First
import Data.Monoid.Last
import Distribution.TestSuite (Progress(Finished), Result(Error, Pass), Test(Test), TestInstance(TestInstance, name, options, run, setOption, tags))
import Prelude (($), (++), (.), Bool, Eq((==)), Char, Int, IO, Show(show), String, foldMap, pure)

assert :: String -> Bool -> String -> Test
assert name' assertion error' =
  let test = TestInstance
        { name      = name'
        , options   = []
        , run       = pure . Finished $
                        if assertion
                          then Pass
                          else Error error'
        , setOption = \_ _ -> pure test
        , tags      = []
        }
  in  Test test

assertEqual :: (Eq a, Show a) => (a, a) -> Test
assertEqual (a, b)
  = assert (show a ++ " == " ++ show b) (a == b) (show a ++ " /= " ++ show b)

firstTest :: Test
firstTest = assertEqual
  (getFirst (foldMap pure [1, 2, 3, 4]), pure 1)

first'0PureTest :: Test
first'0PureTest = assertEqual
  (getFirst' (pure 1 :: First' 0 Int), [])

first'OverloadedStringTest :: Test
first'OverloadedStringTest = assertEqual
  ("abcd" :: First' 2 Char, "ab" :: First' 2 Char)

firstOverloadedStringTest :: Test
firstOverloadedStringTest = assertEqual
  ("abcd" :: First Char, "a" :: First Char)

lastTest :: Test
lastTest = assertEqual
  (getLast (foldMap pure [1, 2, 3, 4]), pure 4)

last'0PureTest :: Test
last'0PureTest = assertEqual
  (getLast' (pure 1 :: Last' 0 Int), [])

last'OverloadedStringTest :: Test
last'OverloadedStringTest = assertEqual
  ("abcd" :: Last' 2 Char, "cd" :: Last' 2 Char)

lastOverloadedStringTest :: Test
lastOverloadedStringTest = assertEqual
  ("abcd" :: Last Char, "d" :: Last Char)

tests :: IO [Test]
tests = pure
  [ firstTest
  , first'0PureTest
  , first'OverloadedStringTest
  , firstOverloadedStringTest
  , lastTest
  , last'0PureTest
  , last'OverloadedStringTest
  , lastOverloadedStringTest
  ]
