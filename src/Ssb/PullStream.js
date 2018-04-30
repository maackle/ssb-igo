"use strict";

var pull = require("pull-stream");
// exports.drain = x => y => console.log("xxx", x, "yyy", y)
//
exports._drain = function (stream) {
  return function (fn) {
    return function (error, success) {
      pull(stream, pull.drain(function (d) {
        return fn(d)();
      }, success));
      return function (ce, cre, cs) {
        return cs();
      };
    };
  };
};
