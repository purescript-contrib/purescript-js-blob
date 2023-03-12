module Test.Main where

import Prelude
import Effect (Effect)
import Test.Js.Blob as Blob

main :: Effect Unit
main = Blob.test

