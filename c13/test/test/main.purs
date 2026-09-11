module Test.Cp13.Main where

import Prelude

import Data.Array (sort, sortBy)
import Data.Array (insert) as A
import Data.Foldable (foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable)

import Effect (Effect)

import Test.QuickCheck ((==?), (<?>))
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.QuickCheck (quickCheck)

import Cp13.Merge (mergeWith, mergePoly, merge)
import Cp13.Sorted (sorted)
import Cp13.Tree (Tree, member, insert, toArray, anywhere)

isSorted :: ∀ a. Ord a => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

ints :: Array Int -> Array Int
ints = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Merge" do
    describe "merge" do
      it "should merge sorted integer arrays (with message)" $ quickCheck
        \xs ys ->
          let
            result = merge (sort xs) (sort ys)
            expected = sort $ xs <> ys
          in
            result == expected
              <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <>
                show expected

      it "should merge sorted integer arrays" $ quickCheck \xs ys ->
        (merge (sorted xs) (sorted ys)) ==? (sort $ sorted xs <> sorted ys)

    describe "mergePoly" do
      it "should merge sorted polymorphic arrays (int)" $ quickCheck \xs ys ->
        (ints $ mergePoly (sorted xs) (sorted ys)) ==?
          (sort $ sorted xs <> sorted ys)

    describe "mergeWith" do
      it "should merge sorted polymorphic arrays with given function" $
        quickCheck \xs ys f ->
          let
            result = map f $ mergeWith (intToBool f)
              (sortBy (compare `on` f) xs)
              (sortBy (compare `on` f) ys)
            expected = map f $ sortBy (compare `on` f) $ xs <> ys
          in
            result ==? expected

  describe "Tree" do
    it "should contain inserted elements" $ quickCheck \t a ->
      member a $ insert a $ treeOfInt t

    it "should fold into sorted array" $ quickCheck \t xs ->
      isSorted $ toArray $ foldr insert t $ ints xs

    describe "anywhere" do
      it "should be true if the predicate passes for any subtree" $
        quickCheck \f g t ->
          anywhere (\s -> f s || g s) t ==?
            (anywhere f (treeOfInt t) || anywhere g t)
