module Test.Cp9.Main where

import Prelude

import Data.Foldable (for_)

import Effect (Effect)

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, unlink)
import Node.Path as Path

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp9.Copy (copyFile)
import Test.Cp9.HTTP (getUrl)

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
