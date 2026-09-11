module Test.Cp9.Solutions where

import Prelude

import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.String (length)

import Effect.Aff (Aff, Error, attempt)

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)

-- ex 1 {{{

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles f1 f2 o = do
  t1 <- readTextFile UTF8 f1
  t2 <- readTextFile UTF8 f2
  writeTextFile UTF8 o (t1 <> t2)

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany fs o =
  writeTextFile UTF8 o =<< foldMap (readTextFile UTF8) fs

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters f =
  attempt $ length <$> readTextFile UTF8 f

-- }}}
