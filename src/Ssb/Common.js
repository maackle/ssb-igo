"use strict";

var R = require("ramda");

var lookup = function lookup(sbot, path) {
  if (name instanceof String) path = [path];
  return R.path(path, sbot);
};

// exports.exposePure = (name, nargs) => {
//   return R.curryN(nargs, lookup(sbot, name))
// }

exports.exposeAff = function (name, nargs) {
  if (nargs == 0) return function (sbot) {
    return function (fail, pass) {
      return lookup(sbot, name)(function (err, data) {
        return err ? fail(err) : pass(data);
      });
    };
  };else if (nargs == 1) return function (sbot) {
    return function (a) {
      return function (fail, pass) {
        return lookup(sbot, name)(a, function (err, data) {
          return err ? fail(err) : pass(data);
        });
      };
    };
  };else if (nargs == 2) return function (sbot) {
    return function (a) {
      return function (b) {
        return function (fail, pass) {
          return lookup(sbot, name)(a, b, function (err, data) {
            return err ? fail(err) : pass(data);
          });
        };
      };
    };
  };else throw "exposeAff not defined for many arguments";
};

exports.exposeEff = function (name, nargs) {
  if (nargs == 0) return function (sbot) {
    return function () {
      return lookup(sbot, name)();
    };
  };else if (nargs == 1) return function (sbot) {
    return function (a) {
      return function () {
        return lookup(sbot, name)(a);
      };
    };
  };else if (nargs == 2) return function (sbot) {
    return function (a) {
      return function (b) {
        return function () {
          return lookup(sbot, name)(a, b);
        };
      };
    };
  };else throw "exposeEff not defined for many arguments";
};
