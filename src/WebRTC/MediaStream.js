// module WebRTC.MediaStream

exports._getUserMedia = function(success) {
    return function(error) {
        return function(constraints) {
            return function() {
                var getUserMedia = navigator.getUserMedia
                    || navigator.webkitGetUserMedia
                    || navigator.mozGetUserMedia;

                return getUserMedia.call(
                    navigator,
                    constraints,
                    function(r) { success(r)(); },
                    function(e) { error(e)(); }
                );
            };
        };
    };
};

exports.createObjectURL = function(blob) {
    return function() {
        return URL.createObjectURL(blob);
    };
};

exports.refEq = function(a) {
  return function(b) {
    return a === b;
  }
}

exports._getMediaStreamTrackKind = function(t) {
  return t.kind;
}

exports.getMediaStreamTracks = function(t) {
  return t.getTracks();
}
