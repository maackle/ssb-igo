var pull = require("pull-stream");
var Abortable = require("pull-abortable");

exports._drainWith = function (stream) {
  return function (fn) {
    return function (error, success) {
      var op = function op(d) {
        return fn(d)();
      };
      var abortable = Abortable();
      var tag = Math.random();
      pull(stream, abortable, pull.drain(op, success));
      return function (ce, cre, cs) {
        abortable.abort();
        cs();
      };
    };
  };
};
