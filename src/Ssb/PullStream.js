"use strict";

var pull = require("pull-stream");
var Abortable = require("pull-abortable");
// exports.drain = x => y => console.log("xxx", x, "yyy", y)
//
exports._drain = function (stream) {
  return function (fn) {
    return function (error, success) {
      var op = function op(d) {
        return fn(d)();
      };
      var abortable = Abortable();
      pull(stream, abortable, pull.through(function (x) {
        return console.log("piping thru", x);
      }), pull.drain(op, success));
      return function (ce, cre, cs) {
        console.info("aborting drain");
        abortable.abort();
        cs();
      };
    };
  };
};
