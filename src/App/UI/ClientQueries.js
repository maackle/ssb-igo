'use strict';

var _require = require('../Ssb.Common/foreign');

var exposeAff = _require.exposeAff;
var exposeEff = _require.exposeEff;

exports.getStream = function (client) {
  return function () {
    return client.ssbIgo.streamDb();
  };
};

exports._getDb = function (client) {
  return function (fail, pass) {
    return client.ssbIgo.rawGet(function (err, val) {
      return err ? fail(err) : pass(val);
    });
  };
};
