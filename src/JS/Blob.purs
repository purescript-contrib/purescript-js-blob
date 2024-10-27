module JS.Blob
  ( Blob
  , BlobEnding(..)
  , BlobOptions
  , ByteIdx
  , EndByte(..)
  , StartByte(..)
  , fromArrayBuffers
  , fromBlobs
  , fromDataView
  , fromString
  , fromStrings
  , idxFromInt
  , idxFromNumber
  , size
  , slice
  , slice'
  , text
  , toArrayBuffer
  , type_
  ) where

import Control.Applicative ((<#>))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.Number (round)
import Effect (Effect)
import Prelude ((#), (==), (>>>))
import Promise (Promise)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Blob :: Type

data BlobEnding = Transparent | Native

type BlobOptions =
  { "type" :: MediaType
  , endings :: BlobEnding
  }

type BlobOptionsImpl =
  { "type" :: String
  , endings :: String
  }

toBlobOptionsImpl :: BlobOptions -> BlobOptionsImpl
toBlobOptionsImpl { "type": mediaType, endings } =
  { "type": un MediaType mediaType
  , endings: toEndings endings
  }
  where
  toEndings Transparent = "transparent"
  toEndings Native = "native"

foreign import fromStringsImpl :: Array String -> Nullable BlobOptionsImpl -> Blob

-- | Creates a String with the given Mediatype
-- | For example:
-- | ```
-- | myBlob = fromString (unsafeStringify { name: "Carl", age: 25 }) (Just { "type": MediaType "application/json", endings: Transparent})
-- | ```
fromString :: String -> Maybe BlobOptions -> Blob
fromString strs opts = fromStringsImpl [ strs ] (opts <#> toBlobOptionsImpl # toNullable)

-- | Creates a new Blob from one or more strings
fromStrings :: NonEmptyArray String -> Maybe BlobOptions -> Blob
fromStrings strs opts = fromStringsImpl (NonEmptyArray.toArray strs) (opts <#> toBlobOptionsImpl # toNullable)

foreign import typeImpl :: Blob -> String

-- | `MediaType` of the data contained in the `Blob`.
-- | Returns `Nothing` if the `MediaType` is unknown.
type_ :: Blob -> Maybe MediaType
type_ blob =
  let
    blobType = typeImpl blob
  in
    if blobType == "" then Nothing
    else Just (MediaType blobType)

-- | The size (in bytes) of the data contained in the `Blob`.
foreign import size :: Blob -> Int

-- | An index into the Blob indicating the first byte to include in the new Blob.
-- | If you specify a negative value, it's treated as an offset from the end of the
-- | string toward the beginning. For example, -10 would be the 10th from last byte
-- | in the Blob. If you specify a value for start that is larger than the size
-- | of the source Blob, the returned Blob has size 0 and contains no data.
newtype StartByte = StartByte ByteIdx

-- | An index into the Blob indicating the first byte that will *not* be included
-- | in the new Blob (i.e. the byte exactly at this index is not included).
-- | If you specify a negative value, it's treated as an offset from the end of
-- | the string toward the beginning. For example, -10 would be the 10th from
-- | last byte in the Blob. The default value is size.
newtype EndByte = EndByte ByteIdx

foreign import data ByteIdx :: Type

-- | Creates `ByteIdx` from `Int` value
idxFromInt :: Int -> ByteIdx
idxFromInt = toNumber >>> unsafeCoerce

-- | Creates `ByteIdx` from `Number` value using `Math.round`.
idxFromNumber :: Number -> ByteIdx
idxFromNumber = round >>> unsafeCoerce

-- | Creates a new `Blob` object (with specified `MediaType`), containing the
-- | data in the specified range of bytes of the source Blob, by setting .
foreign import sliceImpl ∷ Nullable MediaType -> StartByte -> EndByte -> Blob -> Blob

-- | Creates a new `Blob` object containing the data in the specified range
-- | of bytes of the source Blob.
slice ∷ MediaType -> StartByte -> EndByte -> Blob -> Blob
slice mt = sliceImpl (Nullable.notNull mt)

-- | Creates a new `Blob` object containing the data in the specified range
-- | of bytes of the source Blob.
slice' ∷ StartByte -> EndByte -> Blob -> Blob
slice' = sliceImpl (Nullable.null)

-- | Returns a promise that fulfills with the contents of the Blob decoded as a UTF-8 string.
foreign import text :: Blob -> Effect (Promise String)

-- | Copies the data in the Blob to a new JS ArrayBuffer
foreign import toArrayBuffer :: Blob -> Effect (Promise ArrayBuffer)

foreign import fromArrayBuffersImpl :: NonEmptyArray ArrayBuffer -> Nullable BlobOptionsImpl -> Blob

-- | Creates a new Blob from one ore more `ArrayBuffer`s
fromArrayBuffers :: NonEmptyArray ArrayBuffer -> Maybe BlobOptions -> Blob
fromArrayBuffers strs opts = fromArrayBuffersImpl strs (opts <#> toBlobOptionsImpl # toNullable)

foreign import fromBlobsImpl :: NonEmptyArray Blob -> Nullable BlobOptionsImpl -> Blob

-- | Creates a new Blob from one ore more `Blob`s
fromBlobs :: NonEmptyArray Blob -> Maybe BlobOptions -> Blob
fromBlobs strs opts = fromBlobsImpl strs (opts <#> toBlobOptionsImpl # toNullable)

foreign import fromDataViewImpl :: NonEmptyArray DataView -> Nullable BlobOptionsImpl -> Blob

-- | Creates a new Blob from one ore more `DataView`s
fromDataView :: NonEmptyArray DataView -> Maybe BlobOptions -> Blob
fromDataView strs opts = fromDataViewImpl strs (opts <#> toBlobOptionsImpl # toNullable)
