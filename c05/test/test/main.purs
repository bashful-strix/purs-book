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
