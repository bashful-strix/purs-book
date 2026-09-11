module Test.Cp11.Solutions where

import Prelude
import PointFree ((~$))

import Control.Alt ((<|>))
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Reader.Trans (ReaderT, ask, local, runReaderT)
import Control.Monad.State (execState, get, modify, put)
import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)

import Data.Array (some)
import Data.Foldable (fold)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.String.CodeUnits (stripPrefix, toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence, sequence_, traverse_)
import Data.Tuple.Nested (type (/\))

import Cp11.Split (Parser)

-- ex 1 {{{

testParens :: String -> Boolean
testParens =
  toCharArray
    >>> traverse_
      ( \c -> modify \t -> t +
          if c == '(' && t >= 0 then 1
          else if c == ')' then -1
          else 0
      )
    >>> (execState ~$ 0)
    >>> eq 0

-- }}}

-- ex 2 {{{

type Level = Int

type Doc = Reader Level String

line :: String -> Doc
line l = do
  i <- ask
  pure $ (power "  " i) <> l

indent :: Doc -> Doc
indent =
  local (_ + 1)

cat :: Array Doc -> Doc
cat =
  map (joinWith "\n") <<< sequence

render :: Doc -> String
render =
  runReader ~$ 0

-- }}}

-- ex 3 {{{

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter =
  traverse_ (tell <<< Additive)

collatz :: Int -> Int /\ Array Int
collatz =
  runWriter <<< collatz' 0
  where
  collatz' i 1 = tell [ 1 ] $> i
  collatz' i n = do
    tell [ n ]
    collatz' (i + 1)
      if n `mod` 2 == 0 then n / 2
      else (3 * n) + 1

-- }}}

-- ex4 {{{

safeDivide :: Int -> Int -> ExceptT String Identity Int
safeDivide _ 0 = throwError "Divide by zero!"
safeDivide a b = pure $ a / b

string :: String -> Parser String
string prefix = do
  s <- get
  tell [ "The state is " <> s ]
  case stripPrefix (Pattern prefix) s of
    Nothing -> throwError [ "Could not parse" ]
    Just suffix -> do
      put suffix
      pure prefix

type Doc' = ReaderT Level (WriterT (Array String) Identity) Unit

line' :: String -> Doc'
line' l = do
  i <- ask
  tell [ (power "  " i) <> l ]

indent' :: Doc' -> Doc'
indent' =
  local (_ + 1)

cat' :: Array Doc' -> Doc'
cat' =
  sequence_

render' :: Doc' -> String
render' =
  joinWith "\n" <<< unwrap <<< execWriterT <<< (runReaderT ~$ 0)

-- }}}

-- ex 5 {{{

asFollowedByBs :: Parser String
asFollowedByBs = do
  as <- some (string "a")
  bs <- some (string "b")
  pure $ fold $ as <> bs

asOrBs :: Parser String
asOrBs =
  fold <$> some (string "a" <|> string "b")

-- }}}
