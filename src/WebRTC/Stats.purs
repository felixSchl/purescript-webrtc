module WebRTC.Stats where

import Data.Either (Either(..))
import Data.Foreign.Index (readProp, class Index)
import Data.Foreign (Foreign, readString, ForeignError(..), F, fail, toForeign,
                      readNullOrUndefined, readInt)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Prelude

data RTCStats
  = RTCStatsCodec
      (BaseRTCStats
        ( payloadType :: Int
        , codec :: String
        , clockRate :: Int
        , channels :: Int
        , parameters :: String
        , implementation :: String
        ))
  | RTCStatsInboundRtp
      (BaseRTCRTPStreamStats
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
  | RTCStatsOutboundRtp
      (BaseRTCRTPStreamStats
        ( packetsSent :: Int
        , bytesSent :: Int
        , targetBitrate :: Number
        , roundTripTime :: Number
        , framesEncoded :: Int
        ))
  | RTCStatsPeerConnection
      (BaseRTCStats
        ( dataChannelsOpened :: Int
        , dataChannelsClosed :: Int
        ))
  | RTCStatsDataChannel
      (BaseRTCStats
        ( label :: String
        , protocol :: String
        , datachannelid :: Int
        , state :: RTCDataChannelState
        , messagesSent :: Int
        , bytesSent :: Int
        , messagesReceived :: Int
        , bytesReceived :: Int
        ))
  | RTCStatsTrack
      (BaseRTCStats
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
  | RTCStatsTransport
      (BaseRTCStats
        ( bytesSent :: Int
        , bytesReceived :: Int
        , rtcpTransportStatsId :: String
        , activeConnection :: Boolean
        , selectedCandidatePairId :: String
        , localCertificateId :: String
        , remoteCertificateId :: String
        ))
  | RTCStatsCandidatePair
      (BaseRTCStats
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
  | RTCStatsLocalCandidate RTCIceCandidateStatsObj
  | RTCStatsRemoteCandidate RTCIceCandidateStatsObj
  | RTCStatsCertificate
      (BaseRTCStats
        ( fingerprint :: String
        , fingerprintAlgorithm :: String
        , base64Certificate :: String
        , issuerCertificateId :: String
        ))

type RTCIceCandidateStatsObj = BaseRTCStats
  ( transportId :: String
  , isRemote :: Boolean
  , ip :: String
  , port :: Int
  , protocol :: String
  , candidateType :: RTCIceCandidateType
  , priority :: Int
  , url :: String
  , deleted :: Boolean -- (default: false)
  )

instance decodeRTCStats :: Decode RTCStats where
  decode o =
   let
    r :: ∀ a. Decode a => String -> F a
    r k = decode =<< readProp k o
   in readProp "type" o >>= readString >>= case _ of
      "codec" -> RTCStatsCodec <$> do
        { timestamp: _
        , id: _
        , payloadType: _
        , codec: _
        , clockRate: _
        , channels: _
        , parameters: _
        , implementation: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "payloadType"
          <*> r "codec"
          <*> r "clockRate"
          <*> r "channels"
          <*> r "parameters"
          <*> r "implementation"
      "inbound-rtp" -> RTCStatsInboundRtp <$> do
        { timestamp: _
        , id: _
        , ssrc: _
        , associateStatsId: _
        , isRemote: _
        , mediaType: _
        , mediaTrackId: _
        , transportId: _
        , codecId: _
        , firCount: _
        , pliCount: _
        , nackCount: _
        , sliCount: _
        , qpSum: _
        , packetsReceived: _
        , bytesReceived: _
        , packetsLost: _
        , jitter: _
        , fractionLost: _
        , packetsDiscarded: _
        , packetsRepaired: _
        , burstPacketsLost: _
        , burstPacketsDiscarded: _
        , burstLossCount: _
        , burstDiscardCount: _
        , burstLossRate: _
        , burstDiscardRate: _
        , gapLossRate: _
        , gapDiscardRate: _
        , framesDecoded: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "ssrc"
          <*> r "associateStatsId"
          <*> r "isRemote"
          <*> r "mediaType"
          <*> r "mediaTrackId"
          <*> r "transportId"
          <*> r "codecId"
          <*> r "firCount"
          <*> r "pliCount"
          <*> r "nackCount"
          <*> r "sliCount"
          <*> r "qpSum"
          <*> r "packetsReceived"
          <*> r "bytesReceived"
          <*> r "packetsLost"
          <*> r "jitter"
          <*> r "fractionLost"
          <*> r "packetsDiscarded"
          <*> r "packetsRepaired"
          <*> r "burstPacketsLost"
          <*> r "burstPacketsDiscarded"
          <*> r "burstLossCount"
          <*> r "burstDiscardCount"
          <*> r "burstLossRate"
          <*> r "burstDiscardRate"
          <*> r "gapLossRate"
          <*> r "gapDiscardRate"
          <*> r "framesDecoded"
      "outbound-rtp" -> RTCStatsOutboundRtp <$> do
        { timestamp: _
        , id: _
        , ssrc: _
        , associateStatsId: _
        , isRemote: _
        , mediaType: _
        , mediaTrackId: _
        , transportId: _
        , codecId: _
        , firCount: _
        , pliCount: _
        , nackCount: _
        , sliCount: _
        , qpSum: _
        , packetsSent: _
        , bytesSent: _
        , targetBitrate: _
        , roundTripTime: _
        , framesEncoded: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "ssrc"
          <*> r "associateStatsId"
          <*> r "isRemote"
          <*> r "mediaType"
          <*> r "mediaTrackId"
          <*> r "transportId"
          <*> r "codecId"
          <*> r "firCount"
          <*> r "pliCount"
          <*> r "nackCount"
          <*> r "sliCount"
          <*> r "qpSum"
          <*> r "packetsSent"
          <*> r "bytesSent"
          <*> r "targetBitrate"
          <*> r "roundTripTime"
          <*> r "framesEncoded"
      "peer-connection" -> RTCStatsPeerConnection <$> do
        { timestamp: _
        , id: _
        , dataChannelsOpened: _
        , dataChannelsClosed: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "dataChannelsOpened"
          <*> r "dataChannelsClosed"
      "data-channel" -> RTCStatsDataChannel <$> do
        { timestamp: _
        , id: _
        , label: _
        , protocol: _
        , datachannelid: _
        , state: _
        , messagesSent: _
        , bytesSent: _
        , messagesReceived: _
        , bytesReceived: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "label"
          <*> r "protocol"
          <*> r "datachannelid"
          <*> r "state"
          <*> r "messagesSent"
          <*> r "bytesSent"
          <*> r "messagesReceived"
          <*> r "bytesReceived"
      "track" -> RTCStatsTrack <$> do
        { timestamp: _
        , id: _
        , trackIdentifier: _
        , remoteSource: _
        , ended: _
        , detached: _
        , ssrcIds: _
        , frameWidth: _
        , frameHeight: _
        , framesPerSecond: _
        , framesSent: _
        , framesReceived: _
        , framesDecoded: _
        , framesDropped: _
        , framesCorrupted: _
        , partialFramesLost: _
        , fullFramesLost: _
        , audioLevel: _
        , echoReturnLoss: _
        , echoReturnLossEnhancement: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "trackIdentifier"
          <*> r "remoteSource"
          <*> r "ended"
          <*> r "detached"
          <*> r "ssrcIds"
          <*> r "frameWidth"
          <*> r "frameHeight"
          <*> r "framesPerSecond"
          <*> r "framesSent"
          <*> r "framesReceived"
          <*> r "framesDecoded"
          <*> r "framesDropped"
          <*> r "framesCorrupted"
          <*> r "partialFramesLost"
          <*> r "fullFramesLost"
          <*> r "audioLevel"
          <*> r "echoReturnLoss"
          <*> r "echoReturnLossEnhancement"
      "transport" -> RTCStatsTransport <$> do
        { timestamp: _
        , id: _
        , bytesSent: _
        , bytesReceived: _
        , rtcpTransportStatsId: _
        , activeConnection: _
        , selectedCandidatePairId: _
        , localCertificateId: _
        , remoteCertificateId: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "bytesSent"
          <*> r "bytesReceived"
          <*> r "rtcpTransportStatsId"
          <*> r "activeConnection"
          <*> r "selectedCandidatePairId"
          <*> r "localCertificateId"
          <*> r "remoteCertificateId"
      "candidate-pair" -> RTCStatsCandidatePair <$> do
        { timestamp: _
        , id: _
        , transportId: _
        , localCandidateId: _
        , remoteCandidateId: _
        , state: _
        , priority: _
        , nominated: _
        , writable: _
        , readable: _
        , bytesSent: _
        , bytesReceived: _
        , totalRoundTripTime: _
        , currentRoundTripTime: _
        , availableOutgoingBitrate: _
        , availableIncomingBitrate: _
        , requestsReceived: _
        , requestsSent: _
        , responsesReceived: _
        , responsesSent: _
        , retransmissionsReceived: _
        , retransmissionsSent: _
        , consentRequestsReceived: _
        , consentRequestsSent: _
        , consentResponsesReceived: _
        , consentResponsesSent: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "transportId"
          <*> r "localCandidateId"
          <*> r "remoteCandidateId"
          <*> r "state"
          <*> r "priority"
          <*> r "nominated"
          <*> r "writable"
          <*> r "readable"
          <*> r "bytesSent"
          <*> r "bytesReceived"
          <*> r "totalRoundTripTime"
          <*> r "currentRoundTripTime"
          <*> r "availableOutgoingBitrate"
          <*> r "availableIncomingBitrate"
          <*> r "requestsReceived"
          <*> r "requestsSent"
          <*> r "responsesReceived"
          <*> r "responsesSent"
          <*> r "retransmissionsReceived"
          <*> r "retransmissionsSent"
          <*> r "consentRequestsReceived"
          <*> r "consentRequestsSent"
          <*> r "consentResponsesReceived"
          <*> r "consentResponsesSent"
      "local-candidate" -> RTCStatsLocalCandidate <$> do
        { timestamp: _
        , id: _
        , transportId: _
        , isRemote: _
        , ip: _
        , port: _
        , protocol: _
        , candidateType: _
        , priority: _
        , url: _
        , deleted: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "transportId"
          <*> r "isRemote"
          <*> r "ip"
          <*> r "port"
          <*> r "protocol"
          <*> r "candidateType"
          <*> r "priority"
          <*> r "url"
          <*> r "deleted"
      "remote-candidate" -> RTCStatsRemoteCandidate <$> do
        { timestamp: _
        , id: _
        , transportId: _
        , isRemote: _
        , ip: _
        , port: _
        , protocol: _
        , candidateType: _
        , priority: _
        , url: _
        , deleted: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "transportId"
          <*> r "isRemote"
          <*> r "ip"
          <*> r "port"
          <*> r "protocol"
          <*> r "candidateType"
          <*> r "priority"
          <*> r "url"
          <*> r "deleted"
      "certificate" -> RTCStatsCertificate <$> do
        { timestamp: _
        , id: _
        , fingerprint: _
        , fingerprintAlgorithm: _
        , base64Certificate: _
        , issuerCertificateId: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "fingerprint"
          <*> r "fingerprintAlgorithm"
          <*> r "base64Certificate"
          <*> r "issuerCertificateId"
      s -> fail $ ForeignError $ "Unknown stats type: " <> show s

type BaseRTCStats r = Record
  ( timestamp :: Number
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

newtype RTCMediaStreamStats = RTCMediaStreamStats (BaseRTCStats
  ( streamIdentifier :: String
  , trackIds :: Array String
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
