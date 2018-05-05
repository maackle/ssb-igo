'use strict';

var R = require('ramda');
var _ = require('lodash');

var lookup = function lookup(sbot, getter) {
  if (!sbot) {
    throw Error('Scuttlebot not initialized!');
  }
  if (_.isString(getter)) getter = [getter];
  if (_.isArray(getter)) {
    return R.path(getter, sbot);
  }
  if (!getter) return sbot;
  return getter(sbot);
};

exports.exposePure = function (arity, getter) {
  if (arity == 0) return function (sbot) {
    return sbot;
  };else return function (sbot) {
    return R.curryN(arity, lookup(sbot, getter));
  };
};

exports.exposeAff = function (arity, getter) {
  if (arity == 0) return function (sbot) {
    return function (fail, pass) {
      return lookup(sbot, getter)(function (err, data) {
        return err ? fail(err) : pass(data);
      });
    };
  };else if (arity == 1) return function (sbot) {
    return function (a) {
      return function (fail, pass) {
        return lookup(sbot, getter)(a, function (err, data) {
          return err ? fail(err) : pass(data);
        });
      };
    };
  };else if (arity == 2) return function (sbot) {
    return function (a) {
      return function (b) {
        return function (fail, pass) {
          return lookup(sbot, getter)(a, b, function (err, data) {
            return err ? fail(err) : pass(data);
          });
        };
      };
    };
  };else throw 'exposeAff not defined for many arguments';
};

exports.exposeEff = function (arity, getter) {
  if (arity == 0) return function (sbot) {
    return function () {
      return lookup(sbot, getter)();
    };
  };else if (arity == 1) return function (sbot) {
    return function (a) {
      return function () {
        return lookup(sbot, getter)(a);
      };
    };
  };else if (arity == 2) return function (sbot) {
    return function (a) {
      return function (b) {
        return function () {
          return lookup(sbot, getter)(a, b);
        };
      };
    };
  };else throw 'exposeEff not defined for many arguments';
};
