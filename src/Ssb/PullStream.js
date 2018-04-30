"use strict";

var pull = require("pull-stream");
// exports.drain = x => y => console.log("xxx", x, "yyy", y)
//
exports._drain = function (stream) {
  return function (fn) {
    return function (error, success) {
      var op = function op(d) {
        return fn(d)();
      };
      pull(stream, pull.through(function (x) {
        return console.log("piping thru", x);
      }), pull.drain(op, success));
      return function (ce, cre, cs) {
        return cs();
      };
    };
  };
};
