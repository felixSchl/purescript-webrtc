module WebRTC.Stats where

import Data.Either (Either(..))
import Data.Foreign.Index (readProp, class Index)
import Data.Foreign (Foreign, readString, ForeignError(..), F, fail, toForeign,
                      readNullOrUndefined, readInt)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Prelude

data RTCStatsType
    = RTCStatsTypeCodec
    | RTCStatsTypeInboundRtp
    | RTCStatsTypeOutboundRtp
    | RTCStatsTypePeerConnection
    | RTCStatsTypeDataChannel
    | RTCStatsTypeTrack
    | RTCStatsTypeTransport
    | RTCStatsTypeCandidatePair
    | RTCStatsTypeLocalCandidate
    | RTCStatsTypeRemoteCandidate
    | RTCStatsTypeCertificate

instance encodeRTCStatsType :: Encode RTCStatsType where
  encode RTCStatsTypeCodec            = encode "codec"
  encode RTCStatsTypeInboundRtp       = encode "inbound-rtp"
  encode RTCStatsTypeOutboundRtp      = encode "outbound-rtp"
  encode RTCStatsTypePeerConnection   = encode "peer-connection"
  encode RTCStatsTypeDataChannel      = encode "data-channel"
  encode RTCStatsTypeTrack            = encode "track"
  encode RTCStatsTypeTransport        = encode "transport"
  encode RTCStatsTypeCandidatePair    = encode "candidate-pair"
  encode RTCStatsTypeLocalCandidate   = encode "local-candidate"
  encode RTCStatsTypeRemoteCandidate  = encode "remote-candidate"
  encode RTCStatsTypeCertificate      = encode "certificate"

instance decodeRTCStatsType :: Decode RTCStatsType where
  decode f = readString f >>= case _ of
    "codec"            -> pure RTCStatsTypeCodec
    "inbound-rtp"      -> pure RTCStatsTypeInboundRtp
    "outbound-rtp"     -> pure RTCStatsTypeOutboundRtp
    "peer-connection"  -> pure RTCStatsTypePeerConnection
    "data-channel"     -> pure RTCStatsTypeDataChannel
    "track"            -> pure RTCStatsTypeTrack
    "transport"        -> pure RTCStatsTypeTransport
    "candidate-pair"   -> pure RTCStatsTypeCandidatePair
    "local-candidate"  -> pure RTCStatsTypeLocalCandidate
    "remote-candidate" -> pure RTCStatsTypeRemoteCandidate
    "certificate"      -> pure RTCStatsTypeCertificate
    s                  -> fail $ ForeignError $ "Unknown stats type: " <> show s

type BaseRTCStats r = Record
  ( timestamp :: Number
  , type :: RTCStatsType
  , id :: String
  | r
  )

type BaseRTCRTPStreamStats r = BaseRTCStats
  ( ssrc :: String
  , associateStatsId :: String
  , isRemote :: Boolean -- (default: false)
  , mediaType :: String
  , mediaTrackId :: String
  , transportId :: String
  , codecId :: String
  , firCount :: Int
  , pliCount :: Int
  , nackCount :: Int
  , sliCount :: Int
  , qpSum :: Int
  | r
  )

newtype RTCCodecStats = RTCCodecStats (BaseRTCStats (
    payloadType :: Int
  , codec :: String
  , clockRate :: Int
  , channels :: Int
  , parameters :: String
  , implementation :: String
  ))

newtype RTCInboundRTPStreamStats = RTCInboundRTPStreamStats (BaseRTCRTPStreamStats
  ( packetsReceived :: Int
  , bytesReceived :: Int
  , packetsLost :: Int
  , jitter :: Number
  , fractionLost :: Number
  , packetsDiscarded :: Int
  , packetsRepaired :: Int
  , burstPacketsLost :: Int
  , burstPacketsDiscarded :: Int
  , burstLossCount :: Int
  , burstDiscardCount :: Int
  , burstLossRate :: Number
  , burstDiscardRate :: Number
  , gapLossRate :: Number
  , gapDiscardRate :: Number
  , framesDecoded :: Int
  ))

newtype RTCOutboundRTPStreamStats = RTCOutboundRTPStreamStats (BaseRTCRTPStreamStats
  ( packetsSent :: Int
  , bytesSent :: Int
  , targetBitrate :: Number
  , roundTripTime :: Number
  , framesEncoded :: Int
  ))

newtype RTCPeerConnectionStats = RTCPeerConnectionStats (BaseRTCStats
  ( dataChannelsOpened :: Int
  , dataChannelsClosed :: Int
  ))

newtype RTCMediaStreamStats = RTCMediaStreamStats (BaseRTCStats
  ( streamIdentifier :: String
  , trackIds :: Array String
  ))

newtype RTCMediaStreamTrackStats = RTCMediaStreamTrackStats (BaseRTCStats
  ( trackIdentifier :: String
  , remoteSource :: Boolean
  , ended :: Boolean
  , detached :: Boolean
  , ssrcIds :: Array String
  , frameWidth :: Int
  , frameHeight :: Int
  , framesPerSecond :: Number
  , framesSent :: Int
  , framesReceived :: Int
  , framesDecoded :: Int
  , framesDropped :: Int
  , framesCorrupted :: Int
  , partialFramesLost :: Int
  , fullFramesLost :: Int
  , audioLevel :: Number
  , echoReturnLoss :: Number
  , echoReturnLossEnhancement :: Number
  ))

data RTCDataChannelState
  = RTCDataChannelStateConnecting
  | RTCDataChannelStateOpen
  | RTCDataChannelStateClosing
  | RTCDataChannelStateClosed

instance encodeRTCDataChannelState :: Encode RTCDataChannelState where
  encode RTCDataChannelStateConnecting = encode "connecting"
  encode RTCDataChannelStateOpen       = encode "open"
  encode RTCDataChannelStateClosing    = encode "closing"
  encode RTCDataChannelStateClosed     = encode "closed"

instance decodeRTCDataChannelState :: Decode RTCDataChannelState where
  decode f = readString f >>= case _ of
    "connecting" -> pure RTCDataChannelStateConnecting
    "open"       -> pure RTCDataChannelStateOpen
    "closing"    -> pure RTCDataChannelStateClosing
    "closed"     -> pure RTCDataChannelStateClosed
    s            -> fail $ ForeignError $ "Unknown rtc data channel state: " <> show s

newtype RTCDataChannelStats = RTCDataChannelStats (BaseRTCStats
  ( label :: String
  , protocol :: String
  , datachannelid :: Int
  , state :: RTCDataChannelState
  , messagesSent :: Int
  , bytesSent :: Int
  , messagesReceived :: Int
  , bytesReceived :: Int
  ))

newtype RTCTransportStats = RTCTransportStats (BaseRTCStats
  ( bytesSent :: Int
  , bytesReceived :: Int
  , rtcpTransportStatsId :: String
  , activeConnection :: Boolean
  , selectedCandidatePairId :: String
  , localCertificateId :: String
  , remoteCertificateId :: String
  ))

newtype RTCIceCandidatePairStats = RTCIceCandidatePairStats (BaseRTCStats
  ( transportId :: String
  , localCandidateId :: String
  , remoteCandidateId :: String
  , state :: RTCStatsIceCandidatePairState
  , priority :: Int
  , nominated :: Boolean
  , writable :: Boolean
  , readable :: Boolean
  , bytesSent :: Int
  , bytesReceived :: Int
  , totalRoundTripTime :: Number
  , currentRoundTripTime :: Number
  , availableOutgoingBitrate :: Number
  , availableIncomingBitrate :: Number
  , requestsReceived :: Int
  , requestsSent :: Int
  , responsesReceived :: Int
  , responsesSent :: Int
  , retransmissionsReceived :: Int
  , retransmissionsSent :: Int
  , consentRequestsReceived :: Int
  , consentRequestsSent :: Int
  , consentResponsesReceived :: Int
  , consentResponsesSent :: Int
  ))

newtype RTCIceCandidateStats = RTCIceCandidateStats (BaseRTCStats
  ( transportId :: String
  , isRemote :: Boolean
  , ip :: String
  , port :: Int
  , protocol :: String
  , candidateType :: RTCIceCandidateType
  , priority :: Int
  , url :: String
  , deleted :: Boolean -- (default: false)
  ))

data RTCIceCandidateType
  = RTCIceCandidateTypeHost
  | RTCIceCandidateTypeSrflx
  | RTCIceCandidateTypePrflx
  | RTCIceCandidateTypeRelay

instance encodeRTCIceCandidateType :: Encode RTCIceCandidateType where
  encode RTCIceCandidateTypeHost  = encode "host"
  encode RTCIceCandidateTypeSrflx = encode "srflx"
  encode RTCIceCandidateTypePrflx = encode "prflx"
  encode RTCIceCandidateTypeRelay = encode "relay"

instance decodeRTCIceCandidateType :: Decode RTCIceCandidateType where
  decode f = readString f >>= case _ of
    "host"  -> pure RTCIceCandidateTypeHost
    "srflx" -> pure RTCIceCandidateTypeSrflx
    "prflx" -> pure RTCIceCandidateTypePrflx
    "relay" -> pure RTCIceCandidateTypeRelay
    s       -> fail $ ForeignError $ "Unknown ice candidate type: " <> show s

data RTCStatsIceCandidatePairState
  = RTCStatsIceCandidatePairStateFrozen
  | RTCStatsIceCandidatePairStateWaiting
  | RTCStatsIceCandidatePairStateInprogress
  | RTCStatsIceCandidatePairStateFailed
  | RTCStatsIceCandidatePairStateSucceeded
  | RTCStatsIceCandidatePairStateCancelled

instance encodeRTCStatsIceCandidatePairState :: Encode RTCStatsIceCandidatePairState where
  encode RTCStatsIceCandidatePairStateFrozen     = encode "frozen"
  encode RTCStatsIceCandidatePairStateWaiting    = encode "waiting"
  encode RTCStatsIceCandidatePairStateInprogress = encode "inprogress"
  encode RTCStatsIceCandidatePairStateFailed     = encode "failed"
  encode RTCStatsIceCandidatePairStateSucceeded  = encode "succeeded"
  encode RTCStatsIceCandidatePairStateCancelled  = encode "cancelled"

instance decodeRTCStatsIceCandidatePairState :: Decode RTCStatsIceCandidatePairState where
  decode f = readString f >>= case _ of
    "frozen"     -> pure RTCStatsIceCandidatePairStateFrozen
    "waiting"    -> pure RTCStatsIceCandidatePairStateWaiting
    "inprogress" -> pure RTCStatsIceCandidatePairStateInprogress
    "failed"     -> pure RTCStatsIceCandidatePairStateFailed
    "succeeded"  -> pure RTCStatsIceCandidatePairStateSucceeded
    "cancelled"  -> pure RTCStatsIceCandidatePairStateCancelled
    s            -> fail $ ForeignError $ "Unknown ice candidate state: " <> show s


newtype RTCCertificateStats = RTCCertificateStats (BaseRTCStats
  ( fingerprint :: String
  , fingerprintAlgorithm :: String
  , base64Certificate :: String
  , issuerCertificateId :: String
  ))
