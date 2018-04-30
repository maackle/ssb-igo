"use strict";

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
    return (console.log(view), view.stream({ live: true }));
  };
};
