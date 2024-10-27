module Test.JS.Blob
  ( test
  ) where

import Prelude

import Data.Array.NonEmpty ((:))
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.Types (DataView)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common as MediaTypes
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import JS.Blob as Blob
import Promise.Aff as Promise
import Test.Assert (assertEqual)

foreign import testDataView :: Effect DataView

test :: Effect Unit
test = launchAff_ do
  log "Testing fromStrings with no options"
  testFromStringsNoOptions
  log "Testing fromStrings with options"
  testFromStringsWithOptions
  log "Testing fromArrayBuffer / toArrayBuffer"
  testFromBlobs
  log "Testing fromDataView"
  testFromDataView
  log "Testing fromBlobs"
  testToFromArrayBuffer
  log "Testing size"
  testSize
  log "Testing slice"
  testSlice
  log "Test type"
  testType
  where

  testFromStringsNoOptions = do
    let
      input = "hello" : (NEA.singleton "world")
      expected = NEA.fold1 input
      blob = Blob.fromStrings input Nothing
    actual <- Promise.toAffE $ Blob.text blob
    liftEffect $ assertEqual { actual, expected }

  testFromStringsWithOptions = do
    let
      input = "{\n\"hello\":\"world\"\n}" : (NEA.singleton "{\n\"hola\":\"mundo\"\n}")
      expected = NEA.fold1 input
      blob = Blob.fromStrings input (Just { "type": MediaTypes.applicationJSON, endings: Blob.Transparent })
    actual <- Promise.toAffE $ Blob.text blob
    liftEffect $ assertEqual { actual, expected }

  testToFromArrayBuffer = do
    let
      expected = "helloworld"
      blob = Blob.fromStrings (NEA.singleton expected) Nothing
    buffer <- Promise.toAffE $ Blob.toArrayBuffer blob
    actual <- Promise.toAffE $ Blob.text $ Blob.fromArrayBuffers (NEA.singleton buffer) Nothing
    liftEffect $ assertEqual { actual, expected }

  testFromBlobs = do
    let
      expected = "helloworld"
      blob1 = Blob.fromStrings (NEA.singleton "hello") Nothing
      blob2 = Blob.fromStrings (NEA.singleton "world") Nothing
      blob = Blob.fromBlobs (blob1 : NEA.singleton blob2) Nothing
    actual <- Promise.toAffE $ Blob.text $ blob
    liftEffect $ assertEqual { actual, expected }

  testFromDataView = do
    typedArray <- liftEffect testDataView
    let
      expected = 10
      blob = Blob.fromDataView (NEA.singleton typedArray) Nothing
      actual = Blob.size $ blob
    liftEffect $ assertEqual { actual, expected }

  testSize = do
    let
      expected = 11
      blob = Blob.fromStrings (NEA.singleton "hello world") Nothing
      actual = Blob.size blob
    liftEffect $ assertEqual { actual, expected }

  testSlice = do
    let
      expected = "ello wor"
      blob = Blob.fromStrings (NEA.singleton "hello world") Nothing
    actual <- Promise.toAffE $ Blob.text $ Blob.slice' (Blob.StartByte $ Blob.idxFromInt 1) (Blob.EndByte $ Blob.idxFromInt 9) blob
    liftEffect $ assertEqual { actual, expected }

  testType = do
    let
      expected = Just $ MediaType "text/plain"
      blob = Blob.fromStrings (NEA.singleton "hello world") (Just { "type": MediaTypes.textPlain, endings: Blob.Transparent })
      actual = Blob.type_ blob
    liftEffect $ assertEqual { actual, expected }
