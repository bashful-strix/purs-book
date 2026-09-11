module Test.Cp9.HTTP where

import Prelude
import Effect.Aff (Aff)
import Fetch (fetch)

getUrl :: String -> Aff String
getUrl url = fetch url {} >>= _.text
