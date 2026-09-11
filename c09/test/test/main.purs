module Test.Cp9.Main where

import Prelude

import Data.Array ((..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)

import Effect (Effect)
import Effect.Exception (message)

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, realpath, unlink)
import Node.Path as Path

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp9.Copy (copyFile)
import Test.Cp9.HTTP (getUrl)
import Test.Cp9.Solutions
  ( concatenateFiles
  , concatenateMany
  , countCharacters
  )

-- node fs read changed since this was written? trailing newlines causing
-- problems all over the place

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  let
    inDir = Path.concat [ "c09", "test", "test", "data" ]
    outDir = Path.concat [ "c09", "test", "test", "data-out" ]

    -- If, for any reason, you want or need to run this test offline, or without
    -- full internet access, you can create this API endpoint locally by installing
    -- http-server (npm i -g http-server) and running it in the "/test/data"
    -- directory (http-server -p 42524)
    reqUrl =
      -- Both http and https work for this API endpoint.
      "https://jsonplaceholder.typicode.com/todos/1"

  -- Clear test output directory
  it "clear" do -- beforeAll/_ is weird with base monads?
    files <- readdir outDir
    for_ files \f -> unlink $ Path.concat [ outDir, f ]

  describe "Chapter Examples" do
    it "copyFile" do
      let
        inFoo = Path.concat [ inDir, "foo.txt" ]
        outFoo = Path.concat [ outDir, "foo.txt" ]

      copyFile inFoo outFoo
      -- Check for valid copy
      inFooTxt <- readTextFile UTF8 inFoo
      outFooTxt <- readTextFile UTF8 outFoo

      outFooTxt `shouldEqual` inFooTxt

    it "getUrl" do
      let
        expectedOutFile = Path.concat [ inDir, "user.txt" ]

      str <- getUrl reqUrl
      -- Check for valid read
      expectedOutTxt <- readTextFile UTF8 expectedOutFile

      -- deal with trailing \n from reading file
      (str <> "\n") `shouldEqual` expectedOutTxt

  describe "Exercise Group - Async" do
    it "concatenateFiles" do
      let
        inFoo = Path.concat [ inDir, "foo.txt" ]
        inBar = Path.concat [ inDir, "bar.txt" ]
        outFooBar = Path.concat [ outDir, "foobar.txt" ]

      concatenateFiles inFoo inBar outFooBar
      -- Check for valid concat
      inFooTxt <- readTextFile UTF8 inFoo
      inBarTxt <- readTextFile UTF8 inBar
      outFooBarTxt <- readTextFile UTF8 outFooBar

      outFooBarTxt `shouldEqual` (inFooTxt <> inBarTxt)

    it "concatenateMany" do
      let
        inFiles = 1 .. 9 <#> \i ->
          Path.concat [ inDir, "many", "file" <> show i <> ".txt" ]
        outFile = Path.concat [ outDir, "many-concat.txt" ]
        expectedOutFile = Path.concat [ inDir, "many-concat.txt" ]

      concatenateMany inFiles outFile
      -- Check for valid concat
      actualOutTxt <- readTextFile UTF8 outFile
      expectedOutTxt <- readTextFile UTF8 expectedOutFile

      actualOutTxt `shouldEqual` expectedOutTxt

    describe "countCharacters" do
      it "exists" do
        chars <- countCharacters $ Path.concat [ inDir, "nb-chars.txt" ]
        lmap message chars `shouldEqual` (Right 42)

      it "missing" do
        absolutePath <- realpath $ Path.concat [ inDir ]
        chars <- countCharacters $ Path.concat [ absolutePath, "foof.txt" ]

        lmap message chars `shouldEqual`
          ( Left
              ( "ENOENT: no such file or directory, open '" <> absolutePath
                  <> Path.sep
                  <> "foof.txt'"
              )
          )
