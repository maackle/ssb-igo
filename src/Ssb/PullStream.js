"use strict";

var pull = require("pull-stream");

exports.drain = function (fn) {
  return function (error, success) {
    pull.drain(fn, success);
  };
};
