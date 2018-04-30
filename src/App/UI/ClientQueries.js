"use strict";

exports.getStream = function getStream(client) {
  return function () {
    return client.ssbIgo.streamDb();
  };
};
