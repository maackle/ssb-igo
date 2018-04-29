'use strict';

var home = require('os-homedir');
var ssbKeys = require('ssb-keys');

exports._defaultConfig = function () {
  var _ref = arguments[0] === undefined ? {} : arguments[0];

  var keys = _ref.keys;
  var path = _ref.path;
  return function () {
    if (!path) path = home() || 'browser';
    if (!keys) keys = ssbKeys.loadOrCreateSync(path + '/secret');
    return {
      keys: keys,
      path: path,
      shs: 'GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=', // TODO change
      sign: null,
      host: 'localhost',
      port: 8008 };
  };
};
