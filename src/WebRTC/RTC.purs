module WebRTC.RTC (
  RTCPeerConnection(..)
, RTCSessionDescription(..)
, RTCSdpType(..)
, IceConfig(..)
, IceEvent(..)
, MediaStreamEvent(..)
, RTCSignalingState(..)
, RTCDataChannel(..)
, RTCIceConnectionState(..)
, newRTCPeerConnection
, addStream
, removeStream
, createOffer
, createAnswer
, setLocalDescription
, setRemoteDescription
, addIceCandidate
, candidateGatheringDone
, createDataChannel
, canTrickleIceCandidates
, send
, onmessageChannel
, oniceconnectionstatechange
, onicecandidate
, onaddstream
, onremovestream
, onsignalingstatechange
, onnegotiationneeded
, getSignalingState
, getIceConnectionState
, rtcSessionDescription
, getStats
, close
, getLocalStreams
, getRemoteStreams
, module Reexports
) where

import Prelude

import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error, throwException, EXCEPTION)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(Right, Left))
import Data.Foreign (F, Foreign, ForeignError(ForeignError), fail, isNull, isUndefined, readArray, readString, toForeign)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Data.Foreign.Index (readProp)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Traversable (traverse)
import WebRTC.Candidate (RTCIceCandidate)
import WebRTC.Candidate as Reexports
import WebRTC.MediaStream (MediaStream, MediaStreamTrack)
import WebRTC.Stats (RTCStats)

foreign import data IceEvent :: Type

readPropMaybe :: ∀ a. Decode a => String -> Foreign -> F (Maybe a)
readPropMaybe k v = do
  p <- readProp k v
  if isNull p || isUndefined p
    then pure Nothing
    else Just <$> decode p


data RTCSignalingState
  = RTCSignalingStateStable
  | RTCSignalingStateHaveLocalOffer
  | RTCSignalingStateHaveRemoteOffer
  | RTCSignalingStateHaveLocalPranswer
  | RTCSignalingStateHaveRemotePranswer
  | RTCSignalingStateClosed
  | RTCSignalingStateUnknown String

derive instance genericRTCSignalingState :: Generic RTCSignalingState
derive instance eqRTCSignalingState :: Eq RTCSignalingState
derive instance ordRTCSignalingState :: Ord RTCSignalingState

instance showRTCSignalingState :: Show RTCSignalingState where
  show = gShow

instance encodeRTCSignalingState :: Encode RTCSignalingState where
  encode RTCSignalingStateStable             = encode "stable"
  encode RTCSignalingStateHaveLocalOffer     = encode "have-local-offer"
  encode RTCSignalingStateHaveRemoteOffer    = encode "have-remote-offer"
  encode RTCSignalingStateHaveLocalPranswer  = encode "have-local-pranswer"
  encode RTCSignalingStateHaveRemotePranswer = encode "have-remote-pranswer"
  encode RTCSignalingStateClosed             = encode "closed"
  encode (RTCSignalingStateUnknown state)    = encode state

instance decodeRTCSignalingState :: Decode RTCSignalingState where
  decode f = readString f >>= case _ of
    "stable"               -> pure RTCSignalingStateStable
    "have-local-offer"     -> pure RTCSignalingStateHaveLocalOffer
    "have-remote-offer"    -> pure RTCSignalingStateHaveRemoteOffer
    "have-local-pranswer"  -> pure RTCSignalingStateHaveLocalPranswer
    "have-remote-pranswer" -> pure RTCSignalingStateHaveRemotePranswer
    "closed"               -> pure RTCSignalingStateClosed
    state                  -> pure $ RTCSignalingStateUnknown state

data RTCIceConnectionState
  = RTCIceConnectionStateNew
  | RTCIceConnectionStateChecking
  | RTCIceConnectionStateConnected
  | RTCIceConnectionStateCompleted
  | RTCIceConnectionStateFailed
  | RTCIceConnectionStateDisconnected
  | RTCIceConnectionStateClosed

derive instance genericRTCIceConnectionState :: Generic RTCIceConnectionState
derive instance eqRTCIceConnectionState :: Eq RTCIceConnectionState
derive instance ordRTCIceConnectionState :: Ord RTCIceConnectionState

instance showRTCIceConnectionState :: Show RTCIceConnectionState where
  show = gShow

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

derive instance genericRTCSdpType :: Generic RTCSdpType
derive instance eqRTCSdpType :: Eq RTCSdpType
derive instance ordRTCSdpType :: Ord RTCSdpType

instance showRTCSdpType :: Show RTCSdpType where
  show = gShow

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

type IceConfig r = Record (
  iceServers :: Array {
      url :: String
    , credential :: Nullable String
    , username :: Nullable String
  }
  | r
  )

foreign import newRTCPeerConnection
  :: forall e r. IceConfig r -> Eff e RTCPeerConnection

foreign import addStream
  :: forall e. MediaStream -> RTCPeerConnection -> Eff e Unit

foreign import removeStream
  :: forall e. MediaStream -> RTCPeerConnection -> Eff e Unit

foreign import _addIceCandidate
  :: forall e. Foreign -> RTCPeerConnection -> Eff e Unit

addIceCandidate
  :: ∀ e
   . RTCIceCandidate
  -> RTCPeerConnection
  -> Eff e Unit
addIceCandidate = _addIceCandidate <<< encode

foreign import candidateGatheringDone
  :: ∀ e
   . RTCPeerConnection
  -> Eff e Unit

type MediaStreamEvent = { stream :: MediaStream }

foreign import onaddstream
  :: forall e. (MediaStreamEvent -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

foreign import onremovestream
  :: forall e. (MediaStreamEvent -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

foreign import _createOffer
  :: forall e. (Foreign -> Eff e Unit) ->
               (Error -> Eff e Unit) ->
               Foreign ->
               RTCPeerConnection ->
               Eff e Unit

type CreateOfferOptions =
  { iceRestart :: Boolean
  }

createOffer :: forall e. CreateOfferOptions -> RTCPeerConnection -> Aff e RTCSessionDescription
createOffer opts pc = do
  f <-
    makeAff \cb ->
      nonCanceler <$ do
        _createOffer (cb <<< Right) (cb <<< Left) (toForeign opts) pc
  case runExcept $ decode f of
       Left err -> throwError (error $ show err)
       Right v  -> pure v

foreign import _createAnswer
  :: ∀ e
   . (Foreign -> Eff e Unit)
  -> (Error -> Eff e Unit)
  -> RTCPeerConnection
  -> Eff e Unit

createAnswer :: forall e. RTCPeerConnection -> Aff e RTCSessionDescription
createAnswer pc = do
  f <-
    makeAff \cb ->
      nonCanceler <$ do
        _createAnswer (cb <<< Right) (cb <<< Left) pc
  case runExcept $ decode f of
       Left err -> throwError (error $ show err)
       Right v  -> pure v

foreign import _setLocalDescription
  :: ∀ e
   . Eff e Unit
  -> (Error -> Eff e Unit)
  -> Foreign
  -> RTCPeerConnection
  -> Eff e Unit

setLocalDescription :: forall e. RTCSessionDescription -> RTCPeerConnection -> Aff e Unit
setLocalDescription desc pc =
  makeAff \cb ->
    nonCanceler <$ do
      _setLocalDescription (cb (Right unit)) (cb <<< Left) (encode desc) pc

foreign import _setRemoteDescription
  :: ∀ e
   . Eff e Unit
  -> (Error -> Eff e Unit)
  -> Foreign
  -> RTCPeerConnection
  -> Eff e Unit

setRemoteDescription :: ∀ e. RTCSessionDescription -> RTCPeerConnection -> Aff e Unit
setRemoteDescription desc pc =
  makeAff \cb ->
    nonCanceler <$ do
      _setRemoteDescription (cb (Right unit)) (cb <<< Left) (encode desc) pc

foreign import data RTCDataChannel :: Type

foreign import createDataChannel
  :: forall e. String ->
               RTCPeerConnection ->
               Eff e RTCDataChannel

foreign import send
  :: forall e. String ->
               RTCDataChannel ->
               Eff e Unit

foreign import close
  :: forall e. RTCPeerConnection ->
               Eff e Unit

foreign import onmessageChannel
  :: forall e. (String -> Eff e Unit) ->
               RTCDataChannel ->
               Eff e Unit

foreign import _oniceconnectionstatechange
  :: forall e. (Foreign -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

oniceconnectionstatechange
  :: forall e. (RTCIceConnectionState -> Eff (exception :: EXCEPTION | e) Unit) ->
               RTCPeerConnection ->
               Eff (exception :: EXCEPTION | e) Unit
oniceconnectionstatechange f = _oniceconnectionstatechange \state ->
  case runExcept $ decode state of
    Right s  -> f s
    Left err -> throwException $ error $ show err

foreign import _onsignalingstatechange
  :: forall e. (Foreign -> Eff e Unit) ->
               RTCPeerConnection ->
               Eff e Unit

onsignalingstatechange
  :: forall e. (RTCSignalingState -> Eff (exception :: EXCEPTION | e) Unit) ->
               RTCPeerConnection ->
               Eff (exception :: EXCEPTION | e) Unit
onsignalingstatechange f = _onsignalingstatechange \state ->
  case runExcept $ decode state of
    Right s  -> f s
    Left err -> throwException $ error $ show err

foreign import onnegotiationneeded
  :: forall e a
   . Eff e Unit
  -> RTCPeerConnection
  -> Eff e Unit

foreign import _onicecandidate
  :: forall e a
   . (a -> Maybe a)
  -> (Maybe a)
  -> (Maybe Foreign -> Eff e Unit)
  -> RTCPeerConnection
  -> Eff e Unit

onicecandidate
  :: ∀ e
   . (Maybe RTCIceCandidate -> Eff (exception :: EXCEPTION | e) Unit)
  -> RTCPeerConnection
  -> Eff (exception :: EXCEPTION | e) Unit
onicecandidate f = _onicecandidate Just Nothing \mCand -> do
  cand <- case mCand of
    Just cand ->
      case runExcept $ decode cand of
        Right cand -> pure $ Just cand
        Left err   -> throwException $ error $ show err
    Nothing -> pure Nothing
  f cand

foreign import _getSignalingState
  :: RTCPeerConnection
  -> Foreign

getSignalingState :: ∀ eff. RTCPeerConnection -> Eff (exception :: EXCEPTION | eff) RTCSignalingState
getSignalingState pc =
  case runExcept $ decode $ _getSignalingState pc of
    Left err -> throwException (error $ show err)
    Right v -> pure v

foreign import _getIceConnectionState
  :: RTCPeerConnection
  -> Foreign

getIceConnectionState :: ∀ eff. RTCPeerConnection -> Eff (exception :: EXCEPTION | eff) RTCIceConnectionState
getIceConnectionState pc =
  case runExcept $ decode $ _getIceConnectionState pc of
    Left err -> throwException (error $ show err)
    Right v -> pure v

foreign import _getStats
  :: ∀ eff
   . (Foreign -> Eff eff Unit)
  -> (Error -> Eff eff Unit)
  -> Foreign
  -> RTCPeerConnection
  -> Eff eff Unit

foreign import _null :: Foreign

getStats
  :: ∀ eff
   . Maybe MediaStreamTrack
  -> RTCPeerConnection
  -> Aff (exception :: EXCEPTION | eff) (Array RTCStats)
getStats mTrack pc = do
  o <-
    makeAff \cb ->
      nonCanceler <$ do
        _getStats (cb <<< Right) (cb <<< Left) (maybe _null toForeign mTrack) pc
  case runExcept $ go o of
       Left err -> throwError (error $ show err)
       Right v  -> pure v
  where
  go o = readArray o >>= traverse decode

foreign import getRemoteStreams :: ∀ eff. RTCPeerConnection -> Eff eff (Array MediaStream)
foreign import getLocalStreams :: ∀ eff. RTCPeerConnection -> Eff eff (Array MediaStream)

canTrickleIceCandidates
  :: ∀ eff
   . RTCPeerConnection
  -> Eff eff Boolean
canTrickleIceCandidates =
  _canTrickleIceCandidates Just Nothing

foreign import _canTrickleIceCandidates
  :: ∀ eff
   . (Boolean -> Maybe Boolean)
  -> Maybe Boolean
  -> RTCPeerConnection
  -> Eff eff Boolean
