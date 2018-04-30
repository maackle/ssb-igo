"use strict";

exports.mkFlumeReducer = function (version) {
  return function (reducer) {
    return function (mapper) {
      return function (initial) {
        var Reduce = require("flumeview-reduce");
        Reduce(version, reducer, mapper, null, initial);
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
