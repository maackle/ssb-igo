'use strict';

var ssbClient = require('ssb-client');
var ssbKeys = require('ssb-keys');

var _require = require('../Ssb.Common/foreign');

var exposeAff = _require.exposeAff;
var exposeEff = _require.exposeEff;
var exposePure = _require.exposePure;

exports.props = exposePure(0, null);

///////////////// SYNC

exports.unboxPrivate = exposeEff(2, ['private', 'unbox']);

//////////////// ASYNC

exports._close = function (client) {
  return function (error, success) {
    client.close(success);
  };
};

exports._getClient = function (config) {
  return function (error, success) {
    config.caps = {
      shs: config.shs,
      sign: config.sign };
    ssbClient(config.keys, config, function (err, client) {
      if (err) error(err);else success(client);
    });
  };
};

exports._publish = function (client) {
  return function (data) {
    return function (error, success) {
      return client.publish(data, function (err, msg) {
        if (err) error(err);else success(msg);
      });
    };
  };
};

exports._publishPrivate = exposeAff(2, ['private', 'publish']);
exports._whoami = exposeAff(0, 'whoami');
// return (ce, cre, cs) => cs()
