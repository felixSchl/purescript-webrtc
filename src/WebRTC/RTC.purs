module WebRTC.RTC (
  RTCPeerConnection(..)
, RTCSessionDescription(..)
, RTCSdpType(..)
, IceConfig(..)
, IceEvent(..)
, MediaStreamEvent(..)
, RTCIceCandidate(..)
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
, createDataChannel
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
, isRelayCandidate
) where

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.List ((:), List(Nil))
import Data.List as L
import Data.Traversable (traverse)
import Data.Generic (class Generic, gShow)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Nullable (Nullable)
import Data.Either (Either(..), fromRight)
import Data.Foreign.Index (readProp, class Index)
import Data.Foreign (Foreign, readString, ForeignError(..), F, fail, toForeign,
                      readNullOrUndefined, readInt, readArray)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Foreign.Keys (keys)
import Data.String as Str
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error, throwException, EXCEPTION)
import Control.Monad.Except (runExcept, throwError)
import Control.Alt ((<|>))
import Partial.Unsafe (unsafePartial)
import Prelude

import WebRTC.MediaStream
import WebRTC.Stats
import WebRTC.Candidate

foreign import data IceEvent :: Type

newtype RTCIceCandidate = RTCIceCandidate
  { sdpMLineIndex :: Maybe Int
  , sdpMid :: Maybe String
  , candidate :: String
  , type :: RTCIceCandidateType
  }

isRelayCandidate :: RTCIceCandidate -> Boolean
isRelayCandidate (RTCIceCandidate x) = x.type == RTCIceCandidateTypeRelay

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

readPropMaybe :: ∀ a. Decode a => String -> Foreign -> F (Maybe a)
readPropMaybe k v = do
  p <- readProp k v
  (Nothing <$ readNullOrUndefined p) <|> (Just <$> decode p)

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
  :: forall e. (Maybe RTCIceCandidate -> Eff (exception :: EXCEPTION | e) Unit) ->
               RTCPeerConnection ->
               Eff (exception :: EXCEPTION | e) Unit
onicecandidate f = _onicecandidate Just Nothing \mCand -> do
  cand <- case mCand of
    Just cand ->
      case runExcept $ decode cand of
        Right cand -> pure $ Just cand
        Left err   -> throwException $ error $ show err
    Nothing -> pure Nothing
  f cand

foreign import _getSignalingState
  :: RTCPeerConnection -> Foreign

getSignalingState :: ∀ eff. RTCPeerConnection -> Eff (exception :: EXCEPTION | eff) RTCSignalingState
getSignalingState pc = case runExcept $ decode $ _getSignalingState pc of
                            Left err -> throwException (error $ show err)
                            Right v -> pure v

foreign import _getIceConnectionState
  :: RTCPeerConnection -> Foreign

getIceConnectionState :: ∀ eff. RTCPeerConnection -> Eff (exception :: EXCEPTION | eff) RTCIceConnectionState
getIceConnectionState pc = case runExcept $ decode $ _getIceConnectionState pc of
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
  o <- makeAff (\e s -> _getStats s e (maybe _null toForeign mTrack) pc)
  case runExcept $ go o of
       Left err -> throwError (error $ show err)
       Right v  -> pure v
  where
  go o = readArray o >>= traverse decode
