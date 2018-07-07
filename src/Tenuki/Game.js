var tenuki = require('tenuki');

var unwrapEff = function unwrapEff(cb) {
  return function () {
    return cb.apply(undefined, arguments)();
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
            },
            submitMarkDeadAt: function submitMarkDeadAt(y, x, stones, cb) {
              callbacks.submitMarkDeadAt({ x: x, y: y })(stones)();
              cb(true);
            }
          };
          var opts = Object.assign({ element: element, player: player, hooks: hooks }, { gameOptions: terms });
          var client = new tenuki.Client(opts);
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
      return game.pass(opts);
    };
  };
};
// exports.playResign = opts => game => () => game.playResign(opts)
exports.playAt = function (opts) {
  return function (_ref) {
    var x = _ref.x,
        y = _ref.y;
    return function (game) {
      return function () {
        return game.playAt(y, x, opts);
      };
    };
  };
};
exports.toggleDead = function (opts) {
  return function (_ref2) {
    var x = _ref2.x,
        y = _ref2.y;
    return function (game) {
      return function () {
        game.toggleDeadAt(y, x, opts);
      };
    };
  };
};
exports.render = function (game) {
  return function () {
    return game.render();
  };
};
exports.isOver = function (game) {
  return game.isOver();
};
exports.getScore = function (game) {
  return game.score();
};
