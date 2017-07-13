module WebRTC.MediaStream (
  MediaStream(..)
, MediaStreamTrack(..)
, MediaStreamTrackKind(..)
, MediaStreamConstraints(..)
, Blob(..)
, USER_MEDIA()
, getUserMedia
, mediaStreamToBlob
, createObjectURL
, getMediaStreamTrackKind
, getMediaStreamTracks
) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff (Eff(), kind Effect)
import Control.Monad.Eff.Exception (Error())
import Partial.Unsafe (unsafePartial)

foreign import refEq :: ∀ a. a -> a -> Boolean

foreign import data MediaStream :: Type

instance eqMediaStream :: Eq MediaStream where
  eq = refEq

foreign import _getUserMedia
  :: forall e. (MediaStream -> Eff e Unit) ->
               (Error -> Eff e Unit) ->
               MediaStreamConstraints ->
               Eff e Unit

foreign import data USER_MEDIA :: Effect

getUserMedia :: forall e. MediaStreamConstraints -> Aff (userMedia :: USER_MEDIA | e) MediaStream
getUserMedia c = makeAff (\e s -> _getUserMedia s e c)

newtype MediaStreamConstraints =
  MediaStreamConstraints { video :: Boolean
                         , audio :: Boolean
                         }

foreign import data Blob :: Type

mediaStreamToBlob :: MediaStream -> Blob
mediaStreamToBlob = unsafeCoerce

foreign import createObjectURL
  :: forall e. Blob -> Eff e String

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
