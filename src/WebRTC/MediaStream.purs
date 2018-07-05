module WebRTC.MediaStream (
  MediaStream(..)
, MediaStreamTrack(..)
, MediaStreamTrackKind(..)
, MediaStreamConstraints(..)
, Blob(..)
, getUserMedia
, mediaStreamToBlob
, createObjectURL
, getMediaStreamTrackKind
, getMediaStreamTracks
) where

import Prelude

import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect (Effect)
import Effect.Exception (Error)
import Data.Either (Either(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

foreign import refEq :: ∀ a. a -> a -> Boolean

foreign import data MediaStream :: Type

instance eqMediaStream :: Eq MediaStream where
  eq = refEq

foreign import _getUserMedia
  :: (MediaStream -> Effect Unit) ->
      (Error -> Effect Unit) ->
      MediaStreamConstraints ->
      Effect Unit

getUserMedia :: MediaStreamConstraints -> Aff MediaStream
getUserMedia constraints =
  makeAff \cb ->
    nonCanceler <$ do
      _getUserMedia (cb <<< Right) (cb <<< Left) constraints

newtype MediaStreamConstraints =
  MediaStreamConstraints { video :: Boolean
                         , audio :: Boolean
                         }

foreign import data Blob :: Type

mediaStreamToBlob :: MediaStream -> Blob
mediaStreamToBlob = unsafeCoerce

foreign import createObjectURL
  :: Blob -> Effect String

foreign import data MediaStreamTrack :: Type

data MediaStreamTrackKind
  = MediaStreamTrackKindVideo
  | MediaStreamTrackKindAudio

derive instance eqMediaStreamTrackKind :: Eq MediaStreamTrackKind

foreign import getMediaStreamTracks :: MediaStream -> Array MediaStreamTrack

foreign import _getMediaStreamTrackKind :: MediaStreamTrack -> String

getMediaStreamTrackKind :: MediaStreamTrack -> MediaStreamTrackKind
getMediaStreamTrackKind t = unsafePartial $ case _getMediaStreamTrackKind t of
  "audio" -> MediaStreamTrackKindAudio
  "video" -> MediaStreamTrackKindVideo
