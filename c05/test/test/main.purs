module Test.Cp5.Main where

import Prelude

import Data.Array ((..), sort)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((***))
import Data.Traversable (sequence_)
import Data.Tuple.Nested ((/\))

import Effect (Effect)

import Cp5.Data.Path (Path(..), filename, root)

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

  , isPrime
  , cartesianProduct
  , triples
  , primeFactors

  , allTrue
  , fibTailRec
  , reverse

  , onlyFiles
  , whereIs
  , largestSmallest
  , largestSmallest'
  , largestSmallest''
  )

fn :: Maybe Int -> Maybe Int -> Maybe Int
fn ma mb = do
  a <- ma
  b <- mb
  pure (a + b)

factors_ :: Int -> Array (Array Int)
factors_ n = do
  i <- 1 .. n
  j <- i .. n
  pure [ i, j ]

factors_' :: Int -> Array (Array Int)
factors_' n = do
  i <- 1 .. n
  i .. n >>= \j -> pure [ i, j ]

-- bind (i .. n) (\j -> pure [ i, j ])

factors_'' :: Int -> Array (Array Int)
factors_'' n =
  1 .. n >>= \i ->
    i .. n >>= \j ->
      pure [ i, j ]

-- bind (1 .. n) \i ->
--   bind (i .. n) \j ->
--     pure [ i, j ]

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

  describe
    "Exercise Group - Flattening, Comprehensions, Do Notation, and Guards"
    do
      describe "Exercise - isPrime" do
        it "0 is not prime" do
          isPrime 0 `shouldEqual` false

        it "1 is not prime" do
          isPrime 1 `shouldEqual` false

        it "2 is prime" do
          isPrime 2 `shouldEqual` true

        it "4 is not prime" do
          isPrime 4 `shouldEqual` false

        it "997 is prime" do
          isPrime 997 `shouldEqual` true

      -- describe "Exercise - isPrime" $
      --   it "0 is not prime" (isPrime 0 `shouldEqual` false) >>= \_ ->
      --     it "4 is not prime" (isPrime 4 `shouldEqual` false) >>= \_ ->
      --       it "997 is prime" (isPrime 997 `shouldEqual` true)

      describe "Exercise - cartesianProduct" do
        let
          testcp label expected arr1 arr2 =
            it label do
              -- Sorting to allow any ordering
              (sort $ cartesianProduct arr1 arr2) `shouldEqual` (sort expected)

        testcp "Left array is empty" [] [] [ "five" ]

        testcp "Right array is empty" [] [ "5" ] []

        testcp "Two singleton arrays"
          [ "5" /\ "five" ]
          [ "5" ]
          [ "five" ]

        testcp "Arrays larger than singletons"
          [ "5" /\ "five", "5" /\ "six", "6" /\ "five", "6" /\ "six" ]
          [ "5", "6" ]
          [ "five", "six" ]

      describe "Exercise - triples" do
        -- Sorting to allow for any ordering
        it "single element array result" do
          (sort $ triples 5) `shouldEqual` (sort [ 3 /\ 4 /\ 5 ])

        it "multiple element array result" do
          (sort $ triples 13) `shouldEqual`
            (sort [ 3 /\ 4 /\ 5, 5 /\ 12 /\ 13, 6 /\ 8 /\ 10 ])

      describe "Exercise - primeFactors" do
        let
          primeFactorsTest :: Int -> Array Int -> _
          primeFactorsTest n xs =
            it (show n) do
              (sort $ primeFactors n) `shouldEqual` (sort xs)

        primeFactorsTest 1 []
        primeFactorsTest 2 [ 2 ]
        primeFactorsTest 3 [ 3 ]
        primeFactorsTest 4 [ 2, 2 ]
        primeFactorsTest 6 [ 3, 2 ]
        primeFactorsTest 18 [ 3, 3, 2 ]
        primeFactorsTest 210 [ 7, 5, 3, 2 ]

  describe "Exercise Group - Folds and Tail Recursion" do
    describe "Exercise - allTrue" do
      it "all elements true"
        $ allTrue [ true, true, true ] `shouldEqual` true

      it "some elements false"
        $ allTrue [ true, false, true ] `shouldEqual` false

    describe "Exercise - fibTailRec" do
      it "Verify 0" do
        fibTailRec 0 `shouldEqual` 0

      it "Verify 9" do
        fibTailRec 9 `shouldEqual` 34

      it "Verify 44" do
        fibTailRec 44 `shouldEqual` 701408733

    describe "Exercise - reverse" do
      it "Empty Array" do
        reverse [] `shouldEqual` ([] :: Array Int)

      it "Singleton Array" do
        reverse [ 1 ] `shouldEqual` [ 1 ]

      it "More than 1 element" do
        reverse [ 1, 2, 3 ] `shouldEqual` [ 3, 2, 1 ]

  describe "Exercise Group - Filesystem" do
    it "Exercise - onlyFiles" do
      (map filename $ onlyFiles root) `shouldEqual`
        [ "/bin/cp"
        , "/bin/ls"
        , "/bin/mv"
        , "/etc/hosts"
        , "/home/user/todo.txt"
        , "/home/user/code/js/test.js"
        , "/home/user/code/haskell/test.hs"
        ]

    describe "Exercise - whereIs" do
      it "locates a file"
        $ (map filename $ whereIs root "ls") `shouldEqual`
            (Just ("/bin/"))

      it "doesn't locate a file"
        $ (map filename $ whereIs root "cat") `shouldEqual`
            Nothing

    describe "Exercise - largestSmallest" do
      let
        testls :: String -> Array String -> Path -> _
        testls label expected path =
          it label do
            -- Sorting to allow any ordering
            (sort $ map filename $ largestSmallest path)
              `shouldEqual` expected
        oneFileDir = Directory "/etc/" [ File "/etc/hosts" 300 ]
        emptyDir = Directory "/etc/" []

      testls "works for root" [ "/etc/hosts", "/home/user/code/js/test.js" ]
        root
      testls "works for a directory with one file" [ "/etc/hosts" ] oneFileDir
      testls "works for an empty directory" [] emptyDir

    describe "Exercise - largestSmallest'" do
      let
        testls label expected path =
          it label do
            (map (filename *** filename) $ largestSmallest' path)
              `shouldEqual` expected
        oneFileDir = Directory "/etc/" [ File "/etc/hosts" 300 ]
        emptyDir = Directory "/etc/" []

      testls "works for root"
        (Just $ "/home/user/code/js/test.js" /\ "/etc/hosts")
        root
      testls "works for a directory with one file"
        (Just $ "/etc/hosts" /\ "/etc/hosts")
        oneFileDir
      testls "works for an empty directory" Nothing emptyDir

    describe "Exercise - largestSmallest''" do
      let
        testls label expected path =
          it label do
            (map (filename *** filename) $ largestSmallest'' path)
              `shouldEqual` expected
        oneFileDir = Directory "/etc/" [ File "/etc/hosts" 300 ]
        emptyDir = Directory "/etc/" []

      testls "works for root"
        (Just $ "/home/user/code/js/test.js" /\ "/etc/hosts")
        root
      testls "works for a directory with one file"
        (Just $ "/etc/hosts" /\ "/etc/hosts")
        oneFileDir
      testls "works for an empty directory" Nothing emptyDir

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
