'use strict';

var ssbClient = require('ssb-client');

var _require = require('../../output/Ssb.Common/foreign');

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

    var _client = null;
    ssbClient(config.keys, config, function (err, client) {
      _client = client;
      if (err) error(err);else success(client);
    });
    return function (cancelError, cancelerError, cancelerSuccess) {
      console.log('CANCEL', _client);
      if (_client) {
        _client.close();
        cancelerSuccess();
      } else {
        cancelError();
      }
    };
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
