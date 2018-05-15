"use strict";

var pull = require("pull-stream");
var Reduce = require("flumeview-reduce");

exports.mkFlumeReducer = function (version) {
  return function (reducer) {
    return function (mapper) {
      return function (initial) {
        return Reduce(version, reducer, mapper, null, initial);
      };
    };
  };
};

exports.mkFlumeReducer1 = function (version) {
  return function (reducer) {
    return function (mapper) {
      return function (codec) {
        return function (initial) {
          return Reduce(version, reducer, mapper, codec, initial);
        };
      };
    };
  };
};

exports.flumeUse = function (sbot) {
  return function (indexName) {
    return function (view) {
      return function () {
        return sbot._flumeUse(indexName, view);
      };
    };
  };
};

exports.liveStream = function (view) {
  return function () {
    return view.stream({ live: true });
  };
};

exports._rawGet = function (view) {
  return view.get;
};
