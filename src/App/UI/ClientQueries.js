"use strict";

var getStream = function getStream(client) {
  return function () {
    return client.ssbIgo.streamDb();
  };
};
