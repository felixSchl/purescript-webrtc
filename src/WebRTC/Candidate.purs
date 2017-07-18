module WebRTC.Candidate where

import Prelude
import Data.Either (Either(..))
import Data.List ((:), List(Nil))
import Data.List as L
import Data.String as Str
import Data.Generic (class Generic, gShow)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Bifunctor (lmap)
import Data.Generic (class Generic)
import Data.Foreign.Index (readProp, class Index, errorAt)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Foreign (Foreign, readString, ForeignError(..), F, fail, toForeign,
                      readNullOrUndefined, readInt)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Control.Alt ((<|>))
import Control.Monad.Except (mapExcept, runExcept, throwError)

data RTCIceCandidateType
  = RTCIceCandidateTypeHost
  | RTCIceCandidateTypeSrflx
  | RTCIceCandidateTypePrflx
  | RTCIceCandidateTypeRelay

derive instance ordRTCIceCandidateType :: Ord RTCIceCandidateType
derive instance eqRTCIceCandidateType :: Eq RTCIceCandidateType
derive instance genericRTCIceCandidateType :: Generic RTCIceCandidateType

instance encodeRTCIceCandidateType :: Encode RTCIceCandidateType where
  encode = encode <<< rtcIceCandidateTypeToString

rtcIceCandidateTypeToString :: RTCIceCandidateType -> String
rtcIceCandidateTypeToString RTCIceCandidateTypeHost  = "host"
rtcIceCandidateTypeToString RTCIceCandidateTypeSrflx = "srflx"
rtcIceCandidateTypeToString RTCIceCandidateTypePrflx = "prflx"
rtcIceCandidateTypeToString RTCIceCandidateTypeRelay = "relay"

instance decodeRTCIceCandidateType :: Decode RTCIceCandidateType where
  decode o = readString o >>= case _ of
    "host"  -> pure RTCIceCandidateTypeHost
    "srflx" -> pure RTCIceCandidateTypeSrflx
    "prflx" -> pure RTCIceCandidateTypePrflx
    "relay" -> pure RTCIceCandidateTypeRelay
    s       -> fail $ ForeignError $ "Unknown ice candidate type: " <> show s

newtype RTCIceCandidate = RTCIceCandidate
  { sdpMLineIndex :: Maybe Int
  , sdpMid :: Maybe String
  , candidate :: String
  , type :: RTCIceCandidateType
  }

derive instance genericRTCIceCandidate :: Generic RTCIceCandidate
derive instance eqRTCIceCandidate :: Eq RTCIceCandidate
derive instance ordRTCIceCandidate :: Ord RTCIceCandidate

instance showRTCIceCandidate :: Show RTCIceCandidate where
  show = gShow

instance encodeRTCIceCandidate :: Encode RTCIceCandidate where
  encode (RTCIceCandidate cand) = toForeign
    { sdpMLineIndex: maybe undefined toForeign cand.sdpMLineIndex
    , sdpMid: maybe undefined toForeign cand.sdpMid
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
                    case runExcept $ decode $ toForeign typ of
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

readPropMaybe k v = do
  p <- readProp k v
  (Nothing <$ readNullOrUndefined p) <|> (Just <$> decode p)

