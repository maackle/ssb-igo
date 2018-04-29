'use strict';

var ssbKeys = require('ssb-keys');

exports._loadOrCreateSync = function (filename) {
  return function () {
    return ssbKeys.loadOrCreateSync(filename);
  };
};
