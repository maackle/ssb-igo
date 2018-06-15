'use strict';

var tenuki = require('tenuki');

var unwrapEff = function unwrapEff(cb) {
  return function () {
    for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
      args[_key] = arguments[_key];
    }

    return cb.apply(undefined, args)();
  };
};

exports._createGame = function (element) {
  return function (terms) {
    return function () {
      var game = new tenuki.Game(Object.assign({ element: element }, terms));
      return game;
    };
  };
};

exports._createClient = function (element) {
  return function (terms) {
    return function (player) {
      return function (callbacks) {
        return function () {
          var hooks = {
            submitPlay: function submitPlay(y, x, cb) {
              callbacks.submitPlay({ x: x, y: y })();
              cb(true);
            }
          };

          var client = new tenuki.Client(Object.assign({ element: element, player: player, hooks: hooks }, terms));
          return client;
        };
      };
    };
  };
};

exports.setMoveCallback = function (game) {
  return function (cb) {
    return function () {
      game.callbacks.postMove = unwrapEff(cb);
    };
  };
};

exports.getGame = function (client) {
  return client._game;
};
exports.currentState = function (game) {
  return game.currentState();
};

exports.playPass = function (opts) {
  return function (game) {
    return function () {
      return game.playPass(opts);
    };
  };
};
exports.playResign = function (opts) {
  return function (game) {
    return function () {
      return game.playResign(opts);
    };
  };
};
exports.playAt = function (opts) {
  return function (_ref) {
    var x = _ref.x;
    var y = _ref.y;
    return function (game) {
      return function () {
        return game.playAt(y, x, opts);
      };
    };
  };
};
exports.render = function (game) {
  return function () {
    return game.render();
  };
};
