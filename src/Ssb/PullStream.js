"use strict";

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
      console.log("listen to ", tag);
      pull(stream, abortable, pull.through(function (x) {
        return console.log("piping thru", tag, x);
      }), pull.drain(op, success));
      return function (ce, cre, cs) {
        console.info("aborting drain");
        abortable.abort();
        cs();
      };
    };
  };
};
