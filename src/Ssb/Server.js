'use strict';

var scuttlebot = require('scuttlebot');

var _require = require('../Ssb.Common/foreign');

var exposeAff = _require.exposeAff;
var exposeEff = _require.exposeEff;

exports._sbotBuilder = function (plugins) {
  return function () {
    console.log('once');
    var r = function r(sbot, p) {
      return sbot.use(p);
    };
    var create = plugins.reduce(r, scuttlebot);
    return function (config) {
      return function () {
        return create(config);
      };
    };
  };
};
exports._startSbot = scuttlebot;
exports.requirePlugin = function (name) {
  return function () {
    return require(name);
  };
};
exports.toPlugin = function (a) {
  return a;
};

exports._createFeed = exposeEff(0, 'createFeed');
exports._createFeed1 = exposeEff(1, 'createFeed');
exports._createFeed2 = exposeEff(2, 'createFeed');
exports.use = exposeEff(1, 'use');

exports._whoami = exposeAff(0, 'whoami');
