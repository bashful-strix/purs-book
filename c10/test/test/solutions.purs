module Test.Cp10.Solutions where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , Json
  , JsonDecodeError(..)
  , decodeJson
  , encodeJson
  , parseJson
  )
import Data.Argonaut.Decode.Decoders (decodeArray, decodeInt, decodeString)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Encoders (encodeInt, encodeString)
import Data.Argonaut.Encode.Generic (genericEncodeJson)

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Function.Uncurried (Fn3)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Show.Generic (genericShow)

import Test.Cp10.Examples (Complex, Undefined)

-- ex 1 {{{

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number

-- }}}

-- ex 2 {{{

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

-- }}}

-- ex 3 {{{

type Quadratic =
  { a :: Number
  , b :: Number
  , c :: Number
  }

foreign import quadraticRootsImpl
  :: (∀ a. a -> a -> Pair a) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

foreign import toMaybeIml
  :: ∀ a. (∀ x. x -> Maybe x) -> (∀ x. Maybe x) -> Undefined a -> Maybe a

toMaybe :: ∀ a. Undefined a -> Maybe a
toMaybe = toMaybeIml Just Nothing

-- }}}

-- ex 4 {{{

foreign import valuesOfMapImpl :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = decodeJson <<< valuesOfMapImpl <<< encodeJson

valuesOfMapGeneric
  :: ∀ k v
   . Ord k
  => Ord v
  => EncodeJson k
  => EncodeJson v
  => DecodeJson v
  => Map k v
  -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = decodeJson <<< valuesOfMapImpl <<< encodeJson

foreign import quadraticRootsJsonImpl :: Quadratic -> Json

quadraticRootsSet :: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet = decodeJson <<< quadraticRootsJsonImpl

newtype Paired a = Paired (Pair a)

unPaired :: ∀ a. Paired a -> Pair a
unPaired (Paired p) = p

instance DecodeJson a => DecodeJson (Paired a) where
  decodeJson json = decodeArray Right json >>= case _ of
    [ a, b ] -> Paired <$> (Pair <$> decodeJson a <*> decodeJson b)
    _ -> Left $ TypeMismatch "Paired"

quadraticRootsSafe :: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe = map unPaired <<< decodeJson <<< quadraticRootsJsonImpl

parseAndDecodeArray2D :: String -> Either JsonDecodeError (Array (Array Int))
parseAndDecodeArray2D = decodeJson <=< parseJson

data Tree a = Leaf a | Branch (Tree a) (Tree a)

derive instance Eq a => Eq (Tree a)
instance Show a => Show (Tree a) where
  show a = genericShow a

derive instance Generic (Tree a) _
instance DecodeJson a => DecodeJson (Tree a) where
  decodeJson a = genericDecodeJson a

instance EncodeJson a => EncodeJson (Tree a) where
  encodeJson a = genericEncodeJson a

data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

derive instance Generic IntOrString _
derive instance Eq IntOrString
instance Show IntOrString where
  show a = genericShow a

instance DecodeJson IntOrString where
  decodeJson a =
    (IntOrString_Int <$> decodeInt a) <|>
      (IntOrString_String <$> decodeString a)

instance EncodeJson IntOrString where
  encodeJson (IntOrString_Int i) = encodeInt i
  encodeJson (IntOrString_String s) = encodeString s

-- }}}
