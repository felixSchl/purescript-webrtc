// module WebRTC.RTC

exports.newRTCPeerConnection = function(ice) {
    return function() {
        return new (window.RTCPeerConnection || window.webkitRTCPeerConnection || window.mozRTCPeerConnection)(ice, {
            optional: [
		            { DtlsSrtpKeyAgreement: true }
	          ]
        });
    };
};

exports.addStream = function(stream) {
    return function(pc) {
        return function() {
            pc.addStream(stream);
        };
    };
};

exports.removeStream = function(stream) {
    return function(pc) {
        return function() {
            pc.removeStream(stream);
        };
    };
};

exports.onaddstream = function(f) {
    return function(pc) {
        return function() {
            pc.onaddstream = function(event) {
                f(event)();
            };
        };
    };
};

exports.onremovestream = function(f) {
    return function(pc) {
        return function() {
            pc.onremovestream = function(event) {
                f(event)();
            };
        };
    };
};

exports._createOffer = function(success) {
    return function(error) {
        return function (options) {
            return function(pc) {
                return function() {
                    pc.createOffer(
                        { iceRestart: options.iceRestart,
                          offerToReceiveAudio: 1,
                          offerToReceiveVideo: 1 }
                    ).then(
                        function(desc) {
                            success(desc)();
                        },
                        function(e) {
                            error(e)();
                        }
                    );
                };
            };
        };
    };
};

exports._null = null;

exports._getStats = function(success) {
    return function(error) {
        return function(track) {
            return function(pc) {
                return function() {
                    pc.getStats()
                      .then(
                        function(stats) {
                            var xs = [];
                            stats.forEach(function(x) {
                              xs.push(x);
                            });
                            success(xs)();
                        },
                        function(e) {
                            error(e)();
                        }
                    );
                };
            };
        };
    };
};

exports._createAnswer = function(success) {
    return function(error) {
        return function(pc) {
            return function() {
                pc.createAnswer(
                    function(desc) {
                        success(desc)();
                    },
                    function(e) {
                        error(e)();
                    }
                );
            };
        };
    };
};

exports._setLocalDescription = function(success) {
    return function(error) {
        return function(desc) {
            return function(pc) {
                return function() {
                    pc.setLocalDescription(
                        new RTCSessionDescription(desc),
                        success,
                        function(e) {
                            error(e)();
                        }
                    );
                };
            };
        };
    };
};

exports._setRemoteDescription = function(success) {
    return function(error) {
        return function(desc) {
            return function(pc) {
                return function() {
                    pc.setRemoteDescription(
                        desc,
                        success,
                        function(e) {
                            error(e)();
                        }
                    );
                };
            };
        };
    };
};

exports._addIceCandidate = function(c) {
    return function(pc) {
        return function() {
            pc.addIceCandidate(new RTCIceCandidate(c));
        };
    };
};

exports.newRTCSessionDescription = function(s) {
    return new RTCSessionDescription(s);
};

exports.createDataChannel = function(s) {
    return function(pc) {
        return function() {
            var dc = pc.createDataChannel(s);
            dc.onopen = function() {
                console.log("OPENED");
            }
            return dc;
        };
    };
};

exports.send = function(s) {
    return function(dc) {
        return function() {
            if (dc.readyState != "open") return;
            dc.send(s);
        };
    };
};

exports.close = function(pc) {
    return function() {
        pc.close()
    };
};

exports.onmessageChannel = function(f) {
    return function(dc) {
        return function() {
            dc.onmessage = function(m) {
                f(m)();
            };
        };
    };
};

exports._oniceconnectionstatechange = function(f) {
    return function(pc) {
        return function() {
            pc.oniceconnectionstatechange = function() {
                f(pc.iceConnectionState)();
            };
        };
    };
};

exports._onsignalingstatechange = function(f) {
    return function(pc) {
        return function() {
            pc.onsignalingstatechange = function(evt) {
                f(pc.signalingState)();
            };
        };
    };
};

exports._onicecandidate = function(Just) {
  return function(Nothing) {
    return function(f) {
      return function(pc) {
        return function() {
          pc.onicecandidate = function(event) {
            if (event.candidate) {
              f(Just(event.candidate))();
            } else {
              f(Nothing)();
            }
          };
        };
      };
    };
  };
};

exports.onnegotiationneeded = function(f) {
  return function(pc) {
    return function() {
      pc.onnegotiationneeded = function(event) {
        f();
      };
    };
  };
};

exports._getSignalingState = function(pc) {
  return pc.signalingState;
};

exports._getIceConnectionState = function(pc) {
  return pc.iceConnectionState;
};

exports.getLocalStreams = function(pc) {
  return function() {
    return pc.getLocalStreams();
  }
};

exports.getRemoteStreams = function(pc) {
  return function() {
    return pc.getRemoteStreams();
  }
};

exports.candidateGatheringDone = function(pc) {
  return function() {
    pc.addIceCandidate(null);
    return {}
  }
}

exports._canTrickleIceCandidates = function(just) {
  return function(nothing) {
    return function(pc) {
      return function() {
        if (pc.canTrickleIceCandidates != null) {
          return just(!!pc.canTrickleIceCandidates)
        } else {
          return nothing
        }
      }
    }
  }
}
