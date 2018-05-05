
const R = require('ramda')
const _ = require('lodash')

const lookup = (sbot, getter) => {
  if (!sbot) {
    throw Error("Scuttlebot not initialized!")
  }
  if (_.isString(getter)) getter = [getter]
  if (_.isArray(getter)) {
    return R.path(getter, sbot)
  }
  if (!getter) return sbot
  return getter(sbot)
}

exports.exposePure = (arity, getter) => {
  if (arity == 0)
    return sbot => sbot
  else
    return sbot => R.curryN(arity, lookup(sbot, getter))
}

exports.exposeAff = (arity, getter) => {
  if (arity == 0)
    return sbot => (fail, pass) => lookup(sbot, getter)((err, data) => err ? fail(err) : pass(data))
  else if (arity == 1)
    return sbot => a => (fail, pass) => lookup(sbot, getter)(a, (err, data) => err ? fail(err) : pass(data))
  else if (arity == 2)
    return sbot => a => b => (fail, pass) => lookup(sbot, getter)(a, b, (err, data) => err ? fail(err) : pass(data))
  else
    throw "exposeAff not defined for many arguments"
}

exports.exposeEff = (arity, getter) => {
  if (arity == 0)
    return sbot => () => lookup(sbot, getter)()
  else if (arity == 1)
    return sbot => a => () => lookup(sbot, getter)(a)
  else if (arity == 2)
    return sbot => a => b => () => lookup(sbot, getter)(a, b)
  else
    throw "exposeEff not defined for many arguments"
}
