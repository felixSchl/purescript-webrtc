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

import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect (Effect)
import Effect.Exception (Error, error, throwException)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(Right, Left))
import Foreign (F, Foreign, ForeignError(ForeignError), fail, isNull, isUndefined, readArray, readString, unsafeToForeign)
import Foreign.Class (class Decode, class Encode, encode, decode)
import Foreign.Index (readProp)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Traversable (traverse)
import WebRTC.Candidate (RTCIceCandidate)
import WebRTC.Candidate (RTCIceCandidate(..), RTCIceCandidateType(..), readPropMaybe, rtcIceCandidateTypeFromString, rtcIceCandidateTypeToString) as Reexports
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

derive instance genericRTCSignalingState :: Generic RTCSignalingState _
derive instance eqRTCSignalingState :: Eq RTCSignalingState
derive instance ordRTCSignalingState :: Ord RTCSignalingState

instance showRTCSignalingState :: Show RTCSignalingState where
  show = genericShow

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

derive instance genericRTCIceConnectionState :: Generic RTCIceConnectionState _
derive instance eqRTCIceConnectionState :: Eq RTCIceConnectionState
derive instance ordRTCIceConnectionState :: Ord RTCIceConnectionState

instance showRTCIceConnectionState :: Show RTCIceConnectionState where
  show = genericShow

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

derive instance genericRTCSdpType :: Generic RTCSdpType _
derive instance eqRTCSdpType :: Eq RTCSdpType
derive instance ordRTCSdpType :: Ord RTCSdpType

instance showRTCSdpType :: Show RTCSdpType where
  show = genericShow

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
  encode (RTCSessionDescription desc) = unsafeToForeign { type: encode desc.type
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
  :: forall r. IceConfig r -> Effect RTCPeerConnection

foreign import addStream
  :: MediaStream -> RTCPeerConnection -> Effect Unit

foreign import removeStream
  :: MediaStream -> RTCPeerConnection -> Effect Unit

foreign import _addIceCandidate
  :: Foreign -> RTCPeerConnection -> Effect Unit

addIceCandidate
  :: RTCIceCandidate
  -> RTCPeerConnection
  -> Effect Unit
addIceCandidate = _addIceCandidate <<< encode

foreign import candidateGatheringDone
  :: RTCPeerConnection
  -> Effect Unit

type MediaStreamEvent = { stream :: MediaStream }

foreign import onaddstream
  :: (MediaStreamEvent -> Effect Unit) ->
      RTCPeerConnection ->
      Effect Unit

foreign import onremovestream
  :: (MediaStreamEvent -> Effect Unit) ->
      RTCPeerConnection ->
      Effect Unit

foreign import _createOffer
  :: (Foreign -> Effect Unit) ->
      (Error -> Effect Unit) ->
      Foreign ->
      RTCPeerConnection ->
      Effect Unit

type CreateOfferOptions =
  { iceRestart :: Boolean
  }

createOffer :: CreateOfferOptions -> RTCPeerConnection -> Aff RTCSessionDescription
createOffer opts pc = do
  f <-
    makeAff \cb ->
      nonCanceler <$ do
        _createOffer (cb <<< Right) (cb <<< Left) (unsafeToForeign opts) pc
  case runExcept $ decode f of
       Left err -> throwError (error $ show err)
       Right v  -> pure v

foreign import _createAnswer
  :: (Foreign -> Effect Unit)
  -> (Error -> Effect Unit)
  -> RTCPeerConnection
  -> Effect Unit

createAnswer :: RTCPeerConnection -> Aff RTCSessionDescription
createAnswer pc = do
  f <-
    makeAff \cb ->
      nonCanceler <$ do
        _createAnswer (cb <<< Right) (cb <<< Left) pc
  case runExcept $ decode f of
       Left err -> throwError (error $ show err)
       Right v  -> pure v

foreign import _setLocalDescription
  :: Effect Unit
  -> (Error -> Effect Unit)
  -> Foreign
  -> RTCPeerConnection
  -> Effect Unit

setLocalDescription :: RTCSessionDescription -> RTCPeerConnection -> Aff Unit
setLocalDescription desc pc =
  makeAff \cb ->
    nonCanceler <$ do
      _setLocalDescription (cb (Right unit)) (cb <<< Left) (encode desc) pc

foreign import _setRemoteDescription
  :: Effect Unit
  -> (Error -> Effect Unit)
  -> Foreign
  -> RTCPeerConnection
  -> Effect Unit

setRemoteDescription :: RTCSessionDescription -> RTCPeerConnection -> Aff Unit
setRemoteDescription desc pc =
  makeAff \cb ->
    nonCanceler <$ do
      _setRemoteDescription (cb (Right unit)) (cb <<< Left) (encode desc) pc

foreign import data RTCDataChannel :: Type

foreign import createDataChannel
  :: String ->
     RTCPeerConnection ->
     Effect RTCDataChannel

foreign import send
  :: String ->
     RTCDataChannel ->
     Effect Unit

foreign import close
  :: RTCPeerConnection ->
     Effect Unit

foreign import onmessageChannel
  :: (String -> Effect Unit) ->
      RTCDataChannel ->
      Effect Unit

foreign import _oniceconnectionstatechange
  :: (Foreign -> Effect Unit) ->
      RTCPeerConnection ->
      Effect Unit

oniceconnectionstatechange
  :: (RTCIceConnectionState -> Effect Unit) ->
      RTCPeerConnection ->
      Effect Unit
oniceconnectionstatechange f = _oniceconnectionstatechange \state ->
  case runExcept $ decode state of
    Right s  -> f s
    Left err -> throwException $ error $ show err

foreign import _onsignalingstatechange
  :: (Foreign -> Effect Unit) ->
      RTCPeerConnection ->
      Effect Unit

onsignalingstatechange
  :: (RTCSignalingState -> Effect Unit) ->
      RTCPeerConnection ->
      Effect Unit
onsignalingstatechange f = _onsignalingstatechange \state ->
  case runExcept $ decode state of
    Right s  -> f s
    Left err -> throwException $ error $ show err

foreign import onnegotiationneeded
  :: Effect Unit
  -> RTCPeerConnection
  -> Effect Unit

foreign import _onicecandidate
  :: forall a
   . (a -> Maybe a)
  -> (Maybe a)
  -> (Maybe Foreign -> Effect Unit)
  -> RTCPeerConnection
  -> Effect Unit

onicecandidate
  :: (Maybe RTCIceCandidate -> Effect Unit)
  -> RTCPeerConnection
  -> Effect Unit
onicecandidate f = _onicecandidate Just Nothing \mCand -> do
  cand <- case mCand of
    Just cand ->
      case runExcept $ decode cand of
        Right cand' -> pure $ Just cand'
        Left err   -> throwException $ error $ show err
    Nothing -> pure Nothing
  f cand

foreign import _getSignalingState
  :: RTCPeerConnection
  -> Foreign

getSignalingState :: RTCPeerConnection -> Effect RTCSignalingState
getSignalingState pc =
  case runExcept $ decode $ _getSignalingState pc of
    Left err -> throwException (error $ show err)
    Right v -> pure v

foreign import _getIceConnectionState
  :: RTCPeerConnection
  -> Foreign

getIceConnectionState :: RTCPeerConnection -> Effect RTCIceConnectionState
getIceConnectionState pc =
  case runExcept $ decode $ _getIceConnectionState pc of
    Left err -> throwException (error $ show err)
    Right v -> pure v

foreign import _getStats
  :: (Foreign -> Effect Unit)
  -> (Error -> Effect Unit)
  -> Foreign
  -> RTCPeerConnection
  -> Effect Unit

foreign import _null :: Foreign

getStats
  :: Maybe MediaStreamTrack
  -> RTCPeerConnection
  -> Aff (Array RTCStats)
getStats mTrack pc = do
  o <-
    makeAff \cb ->
      nonCanceler <$ do
        _getStats (cb <<< Right) (cb <<< Left) (maybe _null unsafeToForeign mTrack) pc
  case runExcept $ go o of
       Left err -> throwError (error $ show err)
       Right v  -> pure v
  where
  go o = readArray o >>= traverse decode

foreign import getRemoteStreams :: RTCPeerConnection -> Effect (Array MediaStream)
foreign import getLocalStreams :: RTCPeerConnection -> Effect (Array MediaStream)

canTrickleIceCandidates
  :: RTCPeerConnection
  -> Effect Boolean
canTrickleIceCandidates =
  _canTrickleIceCandidates Just Nothing

foreign import _canTrickleIceCandidates
  :: (Boolean -> Maybe Boolean)
  -> Maybe Boolean
  -> RTCPeerConnection
  -> Effect Boolean
