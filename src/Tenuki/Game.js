'use strict';

var tenuki = require('tenuki');

exports._createGame = function (element) {
  return function (terms) {
    return function () {
      var game = new tenuki.Game(Object.assign({ element: element }, terms));
      return game;
    };
  };
};

exports.setMoveCallback = function (game) {
  return function (cb) {
    return function () {
      game.callbacks.postMove = function () {
        for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
          args[_key] = arguments[_key];
        }

        return cb.apply(undefined, args)();
      };
    };
  };
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
