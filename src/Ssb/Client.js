'use strict';

var ssbClient = require('ssb-client');
var ssbKeys = require('ssb-keys');

var _require = require('../Ssb.Common/foreign');

var exposeAff = _require.exposeAff;
var exposeEff = _require.exposeEff;
var exposePure = _require.exposePure;

exports._close = function (client) {
  return function (error, success) {
    client.close(true);
    console.log('connection closed: ', client.closed);
    success();
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
        console.log('publisheddd: ', err, msg);
        if (err) error(err);else success(msg);
      });
    };
  };
};

exports._publishPrivate = exposeAff(['private', 'publish'], 2);
exports._unboxPrivate = exposeEff(['private', 'unbox'], 1);
exports._whoami = exposeAff('whoami', 0);
// return (ce, cre, cs) => cs()
