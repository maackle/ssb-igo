"use strict";

exports.getStream = function getStream(client) {
  return function () {
    const stream = client.ssbIgo.streamDb();
    console.log('da stream', stream)
    return stream
  };
};
