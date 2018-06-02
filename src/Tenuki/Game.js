'use strict';

var tenuki = require('tenuki');

exports.createGame = function (element) {
  return function () {
    var game = new tenuki.Game({ element: element });
    console.log('TENUKI SETUP', element, game);
    return game;
  };
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
