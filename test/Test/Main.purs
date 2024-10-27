module Test.Main where

import Prelude
import Effect (Effect)
import Test.JS.Blob as Blob

main :: Effect Unit
main = Blob.test
