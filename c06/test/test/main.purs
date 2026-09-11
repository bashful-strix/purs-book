module Test.Cp6.Main where

import Prelude

import Data.Foldable (foldMap, foldl, foldr)
import Data.List (List(..), (:))

import Effect (Effect)

import Partial.Unsafe (unsafePartial)

import Test.Spec (describe, it, parallel, pending)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp6.Solutions
  ( Point(..)

  , Complex(..)
  , Shape(..)

  , NonEmpty(..)
  , Extended(..)
  , OneMore(..)
  , dedupShapes
  , dedupShapesFast

  , unsafeMaximum
  , act
  , Multiply(..)
  , Self(..)
  )

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] $ parallel do
  describe "Chapter Examples" do
    pending "Todo for book maintainers - Add tests for chapter examples"

  describe "Show Me!" do
    it "Show Point" do
      (show $ Point { x: 1.0, y: 2.0 }) `shouldEqual` "(1.0, 2.0)"

  describe "Common Type Classes" do
    let cpx real imaginary = Complex { real, imaginary }

    describe "Show Complex" do
      it "possitve imaginary" do
        (show $ cpx 1.0 2.0) `shouldEqual` "1.0+2.0i"

      it "negative imaginary" do
        (show $ cpx 1.0 (-2.0)) `shouldEqual` "1.0-2.0i"

    describe "Eq Complex" do
      it "equal" do
        cpx 1.0 2.0 `shouldEqual` cpx 1.0 2.0

      it "not equal" do
        cpx 1.0 2.0 `shouldNotEqual` cpx 5.0 2.0

    describe "Semiring Complex" do
      it "add" do
        add (cpx 1.0 2.0) (cpx 3.0 4.0) `shouldEqual` (cpx 4.0 6.0)

      let v = cpx 1.2 3.4
      it "add zero" do
        add v zero `shouldEqual` v

      it "multiply" do
        mul (cpx 1.0 2.0) (cpx 3.0 4.0) `shouldEqual` (cpx (-5.0) 10.0)

      it "multiply one" do
        mul v one `shouldEqual` v

    describe "Ring Complex" do
      it "subtract" do
        sub (cpx 3.0 5.0) (cpx 1.0 2.0) `shouldEqual` (cpx 2.0 3.0)

    describe "Show Shape" do
      it "circle" do
        (show $ Circle (Point { x: 1.0, y: 2.0 }) 3.0)
          `shouldEqual` "(Circle (1.0, 2.0) 3.0)"

      it "rectangle" do
        (show $ Rectangle (Point { x: 1.0, y: 2.0 }) 3.0 4.0)
          `shouldEqual` "(Rectangle (1.0, 2.0) 3.0 4.0)"

      it "line" do
        (show $ Line (Point { x: 1.0, y: 2.0 }) (Point { x: 3.0, y: 4.0 }))
          `shouldEqual` "(Line (1.0, 2.0) (3.0, 4.0))"

      it "text" do
        (show $ Text (Point { x: 1.0, y: 2.0 }) "Hello")
          `shouldEqual` "(Text (1.0, 2.0) \"Hello\")"

  describe "Type Class Constraints" do
    describe "Eq NonEmpty" do
      it "equals" do
        NonEmpty 1 [ 2, 3 ] `shouldEqual` (NonEmpty 1 [ 2, 3 ])

      it "not equals" do
        NonEmpty 2 [ 2, 3 ] `shouldNotEqual` (NonEmpty 1 [ 2, 3 ])

    describe "Semigroup NonEmpty" do
      it "append" do
        (NonEmpty 1 [ 2, 3 ] <> NonEmpty 4 [ 5, 6 ])
          `shouldEqual` (NonEmpty 1 [ 2, 3, 4, 5, 6 ])

    describe "Functor NonEmpty" do
      it "map" do
        (map (_ * 10) $ NonEmpty 1 [ 2, 3 ])
          `shouldEqual` (NonEmpty 10 [ 20, 30 ])

    describe "Ord Extended" do
      -- Type annotation necessary to ensure there is an Ord instance for inner type (Int in this case)
      it "infinity equals infinity" do
        compare Infinite (Infinite :: Extended Int) `shouldEqual` EQ

      it "infinity > finite" do
        (compare Infinite $ Finite 5) `shouldEqual` GT

      it "finite < infinity" do
        compare (Finite 5) Infinite `shouldEqual` LT

      it "finite equals finite" do
        (compare (Finite 5) $ Finite 5) `shouldEqual` EQ

      it "finite > finite" do
        (compare (Finite 6) $ Finite 5) `shouldEqual` GT

      it "finite < finite" do
        (compare (Finite 5) $ Finite 6) `shouldEqual` LT

    describe "Foldable NonEmpty" do
      it "foldl" do
        (foldl (\acc x -> acc * 10 + x) 0 $ NonEmpty 1 [ 2, 3 ])
          `shouldEqual` 123

      it "foldr" do
        (foldr (\x acc -> acc * 10 + x) 0 $ NonEmpty 1 [ 2, 3 ])
          `shouldEqual` 321

      it "foldMap" do
        (foldMap (\x -> show x) $ NonEmpty 1 [ 2, 3 ])
          `shouldEqual` "123"

    describe "Foldable OneMore" do
      it "foldl" do
        (foldl (\acc x -> acc * 10 + x) 0 $ OneMore 1 (2 : 3 : Nil))
          `shouldEqual` 123

      it "foldr" do
        (foldr (\x acc -> acc * 10 + x) 0 $ OneMore 1 (2 : 3 : Nil))
          `shouldEqual` 321

      it "foldMap" do
        (foldMap (\x -> show x) $ OneMore 1 (2 : 3 : Nil))
          `shouldEqual` "123"

    let
      withDups =
        [ Circle (Point { x: 1.0, y: 2.0 }) 3.0
        , Circle (Point { x: 3.0, y: 2.0 }) 3.0
        , Circle (Point { x: 1.0, y: 2.0 }) 3.0
        , Circle (Point { x: 2.0, y: 2.0 }) 3.0
        ]
      noDups =
        [ Circle (Point { x: 1.0, y: 2.0 }) 3.0
        , Circle (Point { x: 3.0, y: 2.0 }) 3.0
        , Circle (Point { x: 2.0, y: 2.0 }) 3.0
        ]

    it "dedupShapes" do
      dedupShapes withDups `shouldEqual` noDups

    it "dedupShapesFast" do
      dedupShapesFast withDups `shouldEqual` noDups

  describe "Multi Parameter Type Classes " do
    it "unsafeMaximum" do
      (unsafePartial $ unsafeMaximum [ 1, 2, 42, 3 ]) `shouldEqual` 42

    let
      m1 = Multiply 3
      m2 = Multiply 4

    -- Getting Multiply Int to work is a warm-up
    describe "Action Multiply Int" do
      let a = 5

      it "act mempty" do
        act (mempty :: Multiply) a `shouldEqual` a

      it "act appended" do
        act (m1 <> m2) a `shouldEqual` (act m1 (act m2 a))

      it "concrete" do
        [ 1, 15, 125 ] `shouldContain` act m1 a

    -- Multiply String is the actual exercise question
    describe "Action Multiply String" do
      let a = "foo"

      it "act mempty" do
        act (mempty :: Multiply) a `shouldEqual` a

      it "act appended" do
        act (m1 <> m2) a `shouldEqual` (act m1 (act m2 a))

      it "concrete" do
        act m1 a `shouldEqual` "foofoofoo"

    describe "Action m (Array a)" do
      describe "Action Multiply (Array Int)" do
        let a = [ 1, 2, 3 ]

        it "act mempty" do
          act (mempty :: Multiply) a `shouldEqual` a

        it "act appended" do
          act (m1 <> m2) a `shouldEqual` (act m1 (act m2 a))

        it "concrete" do
          [ [ 0, 0, 1 ], [ 3, 6, 9 ], [ 1, 8, 27 ] ]
            `shouldContain` act m1 a

      describe "Action Multiply (Array String)" do
        let a = [ "foo", "bar", "baz" ]

        it "act mempty" do
          act (mempty :: Multiply) a `shouldEqual` a

        it "act appended" do
          act (m1 <> m2) a `shouldEqual` (act m1 (act m2 a))

        it "concrete" do
          act m1 a `shouldEqual`
            [ "foofoofoo"
            , "barbarbar"
            , "bazbazbaz"
            ]

    describe "Action m (Self m)" do
      let a = Self m1

      it "act mempty" do
        act (mempty :: Multiply) a `shouldEqual` a

      it "act appended" do
        act (m1 <> m2) a `shouldEqual` (act m1 (act m2 a))

      it "concrete" do
        act m2 a `shouldEqual` (Self (Multiply 12))
