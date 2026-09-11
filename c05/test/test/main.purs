module Test.Cp5.Main where

import Prelude

import Data.Array (sort)
import Data.Traversable (sequence_)
import Data.Tuple.Nested ((/\))

import Effect (Effect)

import Cp5.Data.Path (filename, root)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp5.ChapterExamples
  ( allFiles
  , allFiles'
  , factorial
  , factorialTailRec
  , factors
  , factorsV2
  , factorsV3
  , fib
  , length
  , lengthTailRec
  )

import Test.Cp5.Solutions
  ( isEven
  , countEven

  , squared
  , keepNonNegative
  , keepNonNegativeRewrite
  , (<$?>)
  )
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Chapter Examples" do
    it "factorial" $
      factorial 5 `shouldEqual` 120

    it "fib" $
      fib 9 `shouldEqual` 34

    it "length" $
      length [ 0, 0, 0 ] `shouldEqual` 3

    sequence_ do
      name /\ f <-
        [ "factors" /\ factors
        , "factorsV2" /\ factorsV2
        , "factorsV3" /\ factorsV3
        ]
      n /\ xs <-
        [ 1 /\ [ [ 1, 1 ] ]
        , 2 /\ [ [ 1, 2 ] ]
        , 3 /\ [ [ 1, 3 ] ]
        , 4 /\ [ [ 1, 4 ], [ 2, 2 ] ]
        , 10 /\ [ [ 1, 10 ], [ 2, 5 ] ]
        , 100 /\ [ [ 1, 100 ], [ 2, 50 ], [ 4, 25 ], [ 5, 20 ], [ 10, 10 ] ]
        ]
      pure $ it (name <> " " <> show n) do
        (sort $ map sort f n) `shouldEqual` (sort $ map sort xs)

    it "factorialTailRec" $
      factorialTailRec 5 1 `shouldEqual` 120

    it "lengthTailRec" $
      lengthTailRec [ 0, 0, 0 ] `shouldEqual` 3

    it "allFiles" do
      (filename <$> allFiles root) `shouldEqual` allFileAndDirectoryNames

    it "allFiles'" do
      (filename <$> allFiles' root) `shouldEqual` allFileAndDirectoryNames

  describe "Exercise Group - Recursion" do
    describe "Exercise - isEven" do
      it "0 is even" do
        isEven 0 `shouldEqual` true

      it "1 is odd" do
        isEven 1 `shouldEqual` false

      it "20 is even" do
        isEven 20 `shouldEqual` true

      it "19 is odd" do
        isEven 19 `shouldEqual` false

      it "-1 is odd" do
        isEven (-1) `shouldEqual` false

      it "-20 is even" do
        isEven (-20) `shouldEqual` true

      it "-19 is odd" do
        isEven (-19) `shouldEqual` false

    describe "Exercise - countEven" do
      it "[] has none" do
        countEven [] `shouldEqual` 0

      it "[0] has 1" do
        countEven [ 0 ] `shouldEqual` 1

      it "[1] has 0" do
        countEven [ 1 ] `shouldEqual` 0

      it "[0, 1, 19, 20] has 2" do
        countEven [ 0, 1, 19, 20 ] `shouldEqual` 2

  describe "Exercise Group - Maps, Infix Operators, and Filtering" do
    describe "Exercise - squared" do
      it "Do nothing with empty array" do
        squared [] `shouldEqual` []

      it "Calculate squares" do
        squared [ 0.0, 1.0, 2.0, 3.0, 100.0 ]
          `shouldEqual` [ 0.0, 1.0, 4.0, 9.0, 10000.0 ]

    describe "Exercise - keepNonNegative" do
      it "Do nothing with empty array" do
        keepNonNegative [] `shouldEqual` []

      it "Filter negative numbers" do
        keepNonNegative [ -1.5, -1.0, 0.0, -0.1, 2.0, 3.0, -4.0 ]
          `shouldEqual` [ 0.0, 2.0, 3.0 ]

    describe "Exercise - <$?> infix operator for filter" do
      it "Define <$?> operator for filter" do
        ((_ == 1) <$?> [ 1, 2, 3, 1, 2, 3 ])
          `shouldEqual` [ 1, 1 ]

      it "keepNonNegativeRewrite " do
        keepNonNegativeRewrite [ -1.5, -1.0, 0.0, -0.1, 2.0, 3.0, -4.0 ]
          `shouldEqual` [ 0.0, 2.0, 3.0 ]
allFileAndDirectoryNames :: Array (String)
allFileAndDirectoryNames =
  [ "/"
  , "/bin/"
  , "/bin/cp"
  , "/bin/ls"
  , "/bin/mv"
  , "/etc/"
  , "/etc/hosts"
  , "/home/"
  , "/home/user/"
  , "/home/user/todo.txt"
  , "/home/user/code/"
  , "/home/user/code/js/"
  , "/home/user/code/js/test.js"
  , "/home/user/code/haskell/"
  , "/home/user/code/haskell/test.hs"
  ]
