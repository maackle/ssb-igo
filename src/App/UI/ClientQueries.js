"use strict";

exports.getStream = function (client) {
  return function () {
    return client.ssbIgo.streamDb();
  };
};
