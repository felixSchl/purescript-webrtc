module WebRTC.Candidate where

import Prelude
import Data.Either (Either(..))
import Data.List ((:), List(Nil))
import Data.List as L
import Data.String as Str
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (wrap)
import Foreign.Index (readProp)
import Data.Maybe (Maybe(..), maybe)
import Foreign (F, Foreign, ForeignError(..), fail, isNull, isUndefined, readString, unsafeToForeign)
import Foreign.NullOrUndefined (undefined)
import Foreign.Class (class Decode, class Encode, encode, decode)
import Control.Monad.Except (runExcept)

data RTCIceCandidateType
  = RTCIceCandidateTypeHost
  | RTCIceCandidateTypeSrflx
  | RTCIceCandidateTypePrflx
  | RTCIceCandidateTypeRelay

derive instance ordRTCIceCandidateType :: Ord RTCIceCandidateType
derive instance eqRTCIceCandidateType :: Eq RTCIceCandidateType
derive instance genericRTCIceCandidateType :: Generic RTCIceCandidateType _

instance showRTCIceCandidateType :: Show RTCIceCandidateType where
  show = genericShow

instance encodeRTCIceCandidateType :: Encode RTCIceCandidateType where
  encode = encode <<< rtcIceCandidateTypeToString

rtcIceCandidateTypeToString :: RTCIceCandidateType -> String
rtcIceCandidateTypeToString RTCIceCandidateTypeHost  = "host"
rtcIceCandidateTypeToString RTCIceCandidateTypeSrflx = "srflx"
rtcIceCandidateTypeToString RTCIceCandidateTypePrflx = "prflx"
rtcIceCandidateTypeToString RTCIceCandidateTypeRelay = "relay"

rtcIceCandidateTypeFromString :: String -> Either String RTCIceCandidateType
rtcIceCandidateTypeFromString = case _ of
  "host"  -> pure RTCIceCandidateTypeHost
  "srflx" -> pure RTCIceCandidateTypeSrflx
  "prflx" -> pure RTCIceCandidateTypePrflx
  "relay" -> pure RTCIceCandidateTypeRelay
  s       -> Left $ "Unknown ice candidate type: " <> show s

instance decodeRTCIceCandidateType :: Decode RTCIceCandidateType where
  decode o = readString o >>= \s ->
    case rtcIceCandidateTypeFromString s of
      Right v -> pure v
      Left  e -> fail $ ForeignError e

newtype RTCIceCandidate = RTCIceCandidate
  { sdpMLineIndex :: Maybe Int
  , sdpMid :: Maybe String
  , candidate :: String
  , type :: RTCIceCandidateType
  }

derive instance genericRTCIceCandidate :: Generic RTCIceCandidate _
derive instance eqRTCIceCandidate :: Eq RTCIceCandidate
derive instance ordRTCIceCandidate :: Ord RTCIceCandidate

instance showRTCIceCandidate :: Show RTCIceCandidate where
  show = genericShow

instance encodeRTCIceCandidate :: Encode RTCIceCandidate where
  encode (RTCIceCandidate cand) = unsafeToForeign
    { sdpMLineIndex: maybe undefined unsafeToForeign cand.sdpMLineIndex
    , sdpMid: maybe undefined unsafeToForeign cand.sdpMid
    , candidate: encode cand.candidate
    , type: encode cand.type
    }

instance decodeRTCIceCandidate :: Decode RTCIceCandidate where
  decode o = do
    candidate <- readString =<< readProp "candidate" o
    let mType = case L.fromFoldable $ Str.split (wrap " ") candidate of
                  x:_:_:_:_:_:_:typ:_ -> do
                    _ <- case L.fromFoldable $ Str.split (wrap ":") x of
                      "candidate":_:Nil -> Just unit
                      _ -> Nothing
                    case runExcept $ decode $ unsafeToForeign typ of
                         Left _ -> Nothing
                         Right v -> Just v
                  _ -> Nothing
    _type <- maybe  (fail $ ForeignError $ "Invalid candidate string: " <> show candidate)
                    pure
                    mType
    RTCIceCandidate <$> do
      { candidate, type: _type, sdpMid: _, sdpMLineIndex: _ }
        <$> (readPropMaybe "sdpMid" o)
        <*> (readPropMaybe "sdpMLineIndex" o)

readPropMaybe :: ∀ a. Decode a => String -> Foreign -> F (Maybe a)
readPropMaybe k v = do
  p <- readProp k v
  if isNull p || isUndefined p
    then pure Nothing
    else Just <$> decode p

