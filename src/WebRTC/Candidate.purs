module WebRTC.Candidate where

import Prelude
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Data.Generic (class Generic)
import Data.Foreign.Index (readProp, class Index, errorAt)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Foreign (Foreign, readString, ForeignError(..), F, fail, toForeign,
                      readNullOrUndefined, readInt)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Control.Alt ((<|>))
import Control.Monad.Except (mapExcept)

data RTCIceCandidateType
  = RTCIceCandidateTypeHost
  | RTCIceCandidateTypeSrflx
  | RTCIceCandidateTypePrflx
  | RTCIceCandidateTypeRelay

derive instance ordRTCIceCandidateType :: Ord RTCIceCandidateType
derive instance eqRTCIceCandidateType :: Eq RTCIceCandidateType
derive instance genericRTCIceCandidateType :: Generic RTCIceCandidateType

instance encodeRTCIceCandidateType :: Encode RTCIceCandidateType where
  encode RTCIceCandidateTypeHost  = encode "host"
  encode RTCIceCandidateTypeSrflx = encode "srflx"
  encode RTCIceCandidateTypePrflx = encode "prflx"
  encode RTCIceCandidateTypeRelay = encode "relay"

instance decodeRTCIceCandidateType :: Decode RTCIceCandidateType where
  decode o = readString o >>= case _ of
    "host"  -> pure RTCIceCandidateTypeHost
    "srflx" -> pure RTCIceCandidateTypeSrflx
    "prflx" -> pure RTCIceCandidateTypePrflx
    "relay" -> pure RTCIceCandidateTypeRelay
    s       -> fail $ ForeignError $ "Unknown ice candidate type: " <> show s

