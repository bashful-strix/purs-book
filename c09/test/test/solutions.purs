module Test.Cp9.Solutions where

import Prelude

import Control.Parallel (parOneOf, parTraverse)

import Data.Array (filter, snoc)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)

import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay, message)
import Fetch (fetch)

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath, concat, dirname)

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

-- ex 2 {{{

writeGet :: String -> FilePath -> Aff Unit
writeGet url o = do
  r <- attempt (_.text =<< fetch url {})
  writeTextFile UTF8 o case r of
    Left e -> message e
    Right t -> t

-- }}}

-- ex 3 {{{

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel fs o =
  writeTextFile UTF8 o <<< fold =<< parTraverse (readTextFile UTF8) fs

-- do ts <- parTraverse (readTextFile UTF8) fs
--    writeTextFile UTF8 o (fold ts)

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout t url =
  parOneOf
    [ delay (Milliseconds t) $> Nothing
    , fetch url {} >>= _.text <#> Just
    ]

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles f = do
  t <- readTextFile UTF8 f
  let
    links = filter (not <<< eq "") $ split (Pattern "\n") t
    base = dirname f
  fs <- parTraverse (\p -> recurseFiles $ concat [ base, p ]) links
  pure $ fold fs `snoc` f

-- }}}
