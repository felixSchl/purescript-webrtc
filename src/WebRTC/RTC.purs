module WebRTC.RTC (
  RTCPeerConnection(..)
, RTCSessionDescription(..)
, RTCSdpType(..)
, Ice(..)
, IceEvent(..)
, MediaStreamEvent(..)
, RTCIceCandidate(..)
, RTCDataChannel(..)
, RTCIceConnectionState(..)
, newRTCPeerConnection
, addStream
, onicecandidate
, onaddstream
, createOffer
, createAnswer
, setLocalDescription
, setRemoteDescription
, iceEventCandidate
, addIceCandidate
, createDataChannel
, send
, onmessageChannel
, oniceconnectionstatechange
, rtcSessionDescription
) where

import WebRTC.MediaStream
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept, throwError)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Nullable (Nullable)
import Data.Either (Either(..))
import Data.Foreign.Index (readProp)
import Data.Foreign (Foreign, readString, ForeignError(..), fail, toForeign)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Prelude

data RTCIceConnectionState
  = RTCIceConnectionStateNew
  | RTCIceConnectionStateChecking
  | RTCIceConnectionStateConnected
  | RTCIceConnectionStateCompleted
  | RTCIceConnectionStateFailed
  | RTCIceConnectionStateDisconnected
  | RTCIceConnectionStateClosed

instance encodeRTCIceConnectionState :: Encode RTCIceConnectionState where
  encode RTCIceConnectionStateNew          = encode "new"
  encode RTCIceConnectionStateChecking     = encode "checking"
  encode RTCIceConnectionStateConnected    = encode "connected"
  encode RTCIceConnectionStateCompleted    = encode "completed"
  encode RTCIceConnectionStateFailed       = encode "failed"
  encode RTCIceConnectionStateDisconnected = encode "disconnected"
  encode RTCIceConnectionStateClosed       = encode "closed"

instance decodeRTCIceConnectionState :: Decode RTCIceConnectionState where
  decode f = readString f >>= case _ of
    "new"          -> pure RTCIceConnectionStateNew
    "checking"     -> pure RTCIceConnectionStateChecking
    "connected"    -> pure RTCIceConnectionStateConnected
    "completed"    -> pure RTCIceConnectionStateCompleted
    "failed"       -> pure RTCIceConnectionStateFailed
    "disconnected" -> pure RTCIceConnectionStateDisconnected
    "closed"       -> pure RTCIceConnectionStateClosed
    state          -> fail $ ForeignError $ "Invalid ICE state: " <> show state

data RTCSdpType
  = RTCSdpTypeAnswer
  | RTCSdpTypeOffer
  | RTCSdpTypePranswer
  | RTCSdpTypeRollback

instance encodeRTCSdpType :: Encode RTCSdpType where
  encode RTCSdpTypeAnswer   = encode "answer"
  encode RTCSdpTypeOffer    = encode "offer"
  encode RTCSdpTypePranswer = encode "pranswer"
  encode RTCSdpTypeRollback = encode "rollback"

instance decodeRTCSdpType :: Decode RTCSdpType where
  decode f = readString f >>= case _ of
    "answer"   -> pure RTCSdpTypeAnswer
    "pranswer" -> pure RTCSdpTypePranswer
    "offer"    -> pure RTCSdpTypeOffer
    "rollback" -> pure RTCSdpTypeRollback
    type_      -> fail $ ForeignError $ "Invalid sdp type: " <> show type_

newtype RTCSessionDescription = RTCSessionDescription
  { type :: RTCSdpType
  , sdp :: String
  }

rtcSessionDescription :: RTCSdpType -> String -> RTCSessionDescription
rtcSessionDescription type_ sdp = RTCSessionDescription { type: type_, sdp }

derive instance rtcSessionDescriptionNewtype :: Newtype RTCSessionDescription _

instance encodeRTCSessionDescription :: Encode RTCSessionDescription where
  encode (RTCSessionDescription desc) = toForeign { type: encode desc.type
                                                  , sdp: desc.sdp
                                                  }

instance decodeRTCSessionDescription :: Decode RTCSessionDescription where
  decode f = RTCSessionDescription <$> do
    { type: _, sdp: _ }
      <$> (decode =<< readProp "type" f)
      <*> (readString =<< readProp "sdp" f)

foreign import data RTCPeerConnection :: Type

type Ice = { iceServers :: Array { url :: String } }

foreign import newRTCPeerConnection
  :: forall e. Ice -> Eff e RTCPeerConnection

foreign import addStream
  :: forall e. MediaStream -> RTCPeerConnection -> Eff e Unit

foreign import data IceEvent :: Type

type RTCIceCandidate = { sdpMLineIndex :: Nullable Int
                       , sdpMid :: Nullable String
                       , candidate :: String
                       }

foreign import _iceEventCandidate
  :: forall a. Maybe a ->
               (a -> Maybe a) ->
               IceEvent ->
               Maybe RTCIceCandidate

iceEventCandidate :: IceEvent -> Maybe RTCIceCandidate
iceEventCandidate = _iceEventCandidate Nothing Just

foreign import addIceCandidate
  :: forall e. RTCIceCandidate ->
               RTCPeerConnection ->
               Eff e Unit

foreign import onicecandidate
  :: forall e. (IceEvent -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

type MediaStreamEvent = { stream :: MediaStream }

foreign import onaddstream
  :: forall e. (MediaStreamEvent -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

foreign import _createOffer
  :: forall e. (Foreign -> Eff e Unit) ->
               (Error -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

createOffer :: forall e. RTCPeerConnection -> Aff e RTCSessionDescription
createOffer pc = do
  f <- makeAff (\e s -> _createOffer s e pc)
  case runExcept $ decode f of
       Left err -> throwError (error $ show err)
       Right v  -> pure v

foreign import _createAnswer
  :: forall e. (Foreign -> Eff e Unit) ->
               (Error -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

createAnswer :: forall e. RTCPeerConnection -> Aff e RTCSessionDescription
createAnswer pc = do
  f <- makeAff (\e s -> _createAnswer s e pc)
  case runExcept $ decode f of
       Left err -> throwError (error $ show err)
       Right v  -> pure v

foreign import _setLocalDescription
  :: forall e. Eff e Unit ->
               (Error -> Eff e Unit) ->
               Foreign ->
               RTCPeerConnection ->
               Eff e Unit

setLocalDescription :: forall e. RTCSessionDescription -> RTCPeerConnection -> Aff e Unit
setLocalDescription desc pc = makeAff (\e s -> _setLocalDescription (s unit) e (encode desc) pc)

foreign import _setRemoteDescription
  :: forall e. Eff e Unit ->
               (Error -> Eff e Unit) ->
               Foreign ->
               RTCPeerConnection ->
               Eff e Unit

setRemoteDescription :: forall e. RTCSessionDescription -> RTCPeerConnection -> Aff e Unit
setRemoteDescription desc pc = makeAff (\e s -> _setRemoteDescription (s unit) e (encode desc) pc)

foreign import data RTCDataChannel :: Type

foreign import createDataChannel
  :: forall e. String ->
               RTCPeerConnection ->
               Eff e RTCDataChannel

foreign import send
  :: forall e. String ->
               RTCDataChannel ->
               Eff e Unit

foreign import onmessageChannel
  :: forall e. (String -> Eff e Unit) ->
               RTCDataChannel ->
               Eff e Unit

foreign import _oniceconnectionstatechange
  :: forall e. (String -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

oniceconnectionstatechange
  :: forall e. (RTCIceConnectionState -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit
oniceconnectionstatechange f = _oniceconnectionstatechange \x -> case x of
  "new"          -> f RTCIceConnectionStateNew
  "checking"     -> f RTCIceConnectionStateChecking
  "connected"    -> f RTCIceConnectionStateConnected
  "completed"    -> f RTCIceConnectionStateCompleted
  "failed"       -> f RTCIceConnectionStateFailed
  "disconnected" -> f RTCIceConnectionStateDisconnected
  "closed"       -> f RTCIceConnectionStateClosed
  _              -> pure unit
