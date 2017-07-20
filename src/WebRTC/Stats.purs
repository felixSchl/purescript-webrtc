module WebRTC.Stats where

import Prelude
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Data.Foreign.Index (readProp, class Index, errorAt)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Foreign (Foreign, readString, ForeignError(..), F, fail, toForeign,
                      readNullOrUndefined, readInt, isNull, isUndefined)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Control.Alt ((<|>))
import Control.Monad.Except (mapExcept)
import WebRTC.Candidate

readPropMaybe :: ∀ a. Decode a => String -> Foreign -> F (Maybe a)
readPropMaybe k v = do
  p <- readProp k v
  if isNull p || isUndefined p
    then pure Nothing
    else Just <$> decode p


data RTCStats
  = RTCStatsCodec
      (BaseRTCStats
        ( payloadType :: Int
        , codec :: Maybe String
        , mimeType :: Maybe String
        , clockRate :: Int
        , channels :: Maybe Int
        , parameters :: Maybe String
        , implementation :: Maybe String
        ))
  | RTCStatsStream
      (BaseRTCStats
        ( streamIdentifier :: String
        , trackIds :: Array String
        ))
  | RTCStatsInboundRtp
      (BaseRTCRTPStreamStats
        ( packetsReceived :: Int
        , bytesReceived :: Int
        , packetsLost :: Int
        , jitter :: Maybe Number
        , fractionLost :: Maybe Number
        , packetsDiscarded :: Maybe Int
        , packetsRepaired :: Maybe Int
        , burstPacketsLost :: Maybe Int
        , burstPacketsDiscarded :: Maybe Int
        , burstLossCount :: Maybe Int
        , burstDiscardCount :: Maybe Int
        , burstLossRate :: Maybe Number
        , burstDiscardRate :: Maybe Number
        , gapLossRate :: Maybe Number
        , gapDiscardRate :: Maybe Number
        , framesDecoded :: Maybe Int
        ))
  | RTCStatsOutboundRtp
      (BaseRTCRTPStreamStats
        ( packetsSent :: Int
        , bytesSent :: Int
        , targetBitrate :: Maybe Number
        , roundTripTime :: Maybe Number
        , framesEncoded :: Maybe Int
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
        , ssrcIds :: Maybe (Array String)
        , frameWidth :: Maybe Int
        , frameHeight :: Maybe Int
        , framesPerSecond :: Maybe Number
        , framesSent :: Maybe Int
        , framesReceived :: Maybe Int
        , framesDecoded :: Maybe Int
        , framesDropped :: Maybe Int
        , framesCorrupted :: Maybe Int
        , partialFramesLost :: Maybe Int
        , fullFramesLost :: Maybe Int
        , audioLevel :: Maybe Number
        , echoReturnLoss :: Maybe Number
        , echoReturnLossEnhancement :: Maybe Number
        ))
  | RTCStatsTransport
      (BaseRTCStats
        ( bytesSent :: Int
        , bytesReceived :: Int
        , rtcpTransportStatsId :: Maybe String
        , activeConnection :: Maybe Boolean
        , selectedCandidatePairId :: Maybe String
        , localCertificateId :: Maybe String
        , remoteCertificateId :: Maybe String
        ))
  | RTCStatsCandidatePair
      (BaseRTCStats
        ( transportId :: String
        , localCandidateId :: String
        , remoteCandidateId :: String
        , state :: RTCStatsIceCandidatePairState
        , priority :: Number
        , nominated :: Boolean
        , writable :: Boolean
        , readable :: Maybe Boolean
        , bytesSent :: Int
        , bytesReceived :: Int
        , totalRoundTripTime :: Number
        , currentRoundTripTime :: Maybe Number
        , availableOutgoingBitrate :: Maybe Number
        , availableIncomingBitrate :: Maybe Number
        , requestsReceived :: Maybe Int
        , requestsSent :: Maybe Int
        , responsesReceived :: Maybe Int
        , responsesSent :: Maybe Int
        , retransmissionsReceived :: Maybe Int
        , retransmissionsSent :: Maybe Int
        , consentRequestsReceived :: Maybe Int
        , consentRequestsSent :: Maybe Int
        , consentResponsesReceived :: Maybe Int
        , consentResponsesSent :: Maybe Int
        ))
  | RTCStatsLocalCandidate RTCIceCandidateStatsObj
  | RTCStatsRemoteCandidate RTCIceCandidateStatsObj
  | RTCStatsCertificate
      (BaseRTCStats
        ( fingerprint :: String
        , fingerprintAlgorithm :: String
        , base64Certificate :: String
        , issuerCertificateId :: Maybe String
        ))

type RTCIceCandidateStatsObj = BaseRTCStats
  ( transportId :: String
  , isRemote :: Boolean
  , ip :: String
  , port :: Int
  , protocol :: String
  , candidateType :: RTCIceCandidateType
  , priority :: Number
  , url :: Maybe String
  , deleted :: Boolean -- (default: false)
  )

mapErrorAt prop = mapExcept (lmap (map (errorAt prop)))

instance decodeRTCStats :: Decode RTCStats where
  decode o =
   let
    r :: ∀ a. Decode a => String -> F a
    r k = mapErrorAt k $ decode =<< readProp k o

    rM :: ∀ a. Decode a => String -> F (Maybe a)
    rM k = mapErrorAt k $ readPropMaybe k o
   in r "type" >>= case _ of
      "codec" -> RTCStatsCodec <$> do
        { timestamp: _
        , id: _
        , payloadType: _
        , codec: _
        , mimeType: _
        , clockRate: _
        , channels: _
        , parameters: _
        , implementation: _
        }
          <$> r  "timestamp"
          <*> r  "id"
          <*> r  "payloadType"
          <*> rM "codec"
          <*> rM "mimeType"
          <*> r  "clockRate"
          <*> rM "channels"
          <*> rM "parameters"
          <*> rM "implementation"
      "stream" -> RTCStatsStream <$> do
        { timestamp: _
        , id: _
        , streamIdentifier: _
        , trackIds: _
        }
          <$> r "timestamp"
          <*> r "id"
          <*> r "streamIdentifier"
          <*> r "trackIds"
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
          <$> r  "timestamp"
          <*> r  "id"
          <*> rM "ssrc"
          <*> rM "associateStatsId"
          <*> r  "isRemote"
          <*> r  "mediaType"
          <*> rM "mediaTrackId"
          <*> r  "transportId"
          <*> rM "codecId"
          <*> rM "firCount"
          <*> rM "pliCount"
          <*> rM "nackCount"
          <*> rM "sliCount"
          <*> rM "qpSum"
          <*> r  "packetsReceived"
          <*> r  "bytesReceived"
          <*> r  "packetsLost"
          <*> rM "jitter"
          <*> rM "fractionLost"
          <*> rM "packetsDiscarded"
          <*> rM "packetsRepaired"
          <*> rM "burstPacketsLost"
          <*> rM "burstPacketsDiscarded"
          <*> rM "burstLossCount"
          <*> rM "burstDiscardCount"
          <*> rM "burstLossRate"
          <*> rM "burstDiscardRate"
          <*> rM "gapLossRate"
          <*> rM "gapDiscardRate"
          <*> rM "framesDecoded"
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
          <$> r  "timestamp"
          <*> r  "id"
          <*> rM "ssrc"
          <*> rM "associateStatsId"
          <*> r  "isRemote"
          <*> r  "mediaType"
          <*> rM "mediaTrackId"
          <*> r  "transportId"
          <*> rM "codecId"
          <*> rM "firCount"
          <*> rM "pliCount"
          <*> rM "nackCount"
          <*> rM "sliCount"
          <*> rM "qpSum"
          <*> r  "packetsSent"
          <*> r  "bytesSent"
          <*> rM "targetBitrate"
          <*> rM "roundTripTime"
          <*> rM "framesEncoded"
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
          <$> r  "timestamp"
          <*> r  "id"
          <*> r  "trackIdentifier"
          <*> r  "remoteSource"
          <*> r  "ended"
          <*> r  "detached"
          <*> rM "ssrcIds"
          <*> rM "frameWidth"
          <*> rM "frameHeight"
          <*> rM "framesPerSecond"
          <*> rM "framesSent"
          <*> rM "framesReceived"
          <*> rM "framesDecoded"
          <*> rM "framesDropped"
          <*> rM "framesCorrupted"
          <*> rM "partialFramesLost"
          <*> rM "fullFramesLost"
          <*> rM "audioLevel"
          <*> rM "echoReturnLoss"
          <*> rM "echoReturnLossEnhancement"
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
          <$> r  "timestamp"
          <*> r  "id"
          <*> r  "bytesSent"
          <*> r  "bytesReceived"
          <*> rM "rtcpTransportStatsId"
          <*> rM "activeConnection"
          <*> rM "selectedCandidatePairId"
          <*> rM "localCertificateId"
          <*> rM "remoteCertificateId"
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
          <$> r  "timestamp"
          <*> r  "id"
          <*> r  "transportId"
          <*> r  "localCandidateId"
          <*> r  "remoteCandidateId"
          <*> r  "state"
          <*> r  "priority"
          <*> r  "nominated"
          <*> r  "writable"
          <*> rM "readable"
          <*> r  "bytesSent"
          <*> r  "bytesReceived"
          <*> r  "totalRoundTripTime"
          <*> rM "currentRoundTripTime"
          <*> rM "availableOutgoingBitrate"
          <*> rM "availableIncomingBitrate"
          <*> rM "requestsReceived"
          <*> rM "requestsSent"
          <*> rM "responsesReceived"
          <*> rM "responsesSent"
          <*> rM "retransmissionsReceived"
          <*> rM "retransmissionsSent"
          <*> rM "consentRequestsReceived"
          <*> rM "consentRequestsSent"
          <*> rM "consentResponsesReceived"
          <*> rM "consentResponsesSent"
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
          <$> r  "timestamp"
          <*> r  "id"
          <*> r  "transportId"
          <*> r  "isRemote"
          <*> r  "ip"
          <*> r  "port"
          <*> r  "protocol"
          <*> r  "candidateType"
          <*> r  "priority"
          <*> rM "url"
          <*> r  "deleted"
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
          <$> r  "timestamp"
          <*> r  "id"
          <*> r  "transportId"
          <*> r  "isRemote"
          <*> r  "ip"
          <*> r  "port"
          <*> r  "protocol"
          <*> r  "candidateType"
          <*> r  "priority"
          <*> rM "url"
          <*> r  "deleted"
      "certificate" -> RTCStatsCertificate <$> do
        { timestamp: _
        , id: _
        , fingerprint: _
        , fingerprintAlgorithm: _
        , base64Certificate: _
        , issuerCertificateId: _
        }
          <$> r  "timestamp"
          <*> r  "id"
          <*> r  "fingerprint"
          <*> r  "fingerprintAlgorithm"
          <*> r  "base64Certificate"
          <*> rM "issuerCertificateId"
      s -> fail $ ForeignError $ "Unknown stats type: " <> show s

type BaseRTCStats r = Record
  ( timestamp :: Number
  , id :: String
  | r
  )

type BaseRTCRTPStreamStats r = BaseRTCStats
  ( ssrc :: Maybe Number
  , associateStatsId :: Maybe String
  , isRemote :: Boolean -- (default: false)
  , mediaType :: String
  , mediaTrackId :: Maybe String
  , transportId :: String
  , codecId :: Maybe String
  , firCount :: Maybe Int
  , pliCount :: Maybe Int
  , nackCount :: Maybe Int
  , sliCount :: Maybe Int
  , qpSum :: Maybe Int
  | r
  )

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
    "frozen"      -> pure RTCStatsIceCandidatePairStateFrozen
    "waiting"     -> pure RTCStatsIceCandidatePairStateWaiting
    "in-progress" -> pure RTCStatsIceCandidatePairStateInprogress
    "inprogress"  -> pure RTCStatsIceCandidatePairStateInprogress
    "failed"      -> pure RTCStatsIceCandidatePairStateFailed
    "succeeded"   -> pure RTCStatsIceCandidatePairStateSucceeded
    "cancelled"   -> pure RTCStatsIceCandidatePairStateCancelled
    s             -> fail $ ForeignError $ "Unknown ice candidate state: " <> show s
