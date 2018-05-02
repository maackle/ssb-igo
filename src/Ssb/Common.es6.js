
const R = require('ramda')

const lookup = (sbot, path) => {
  if (name instanceof String) path = [path]
  return R.path(path, sbot)
}

// exports.exposePure = (name, nargs) => {
//   return R.curryN(nargs, lookup(sbot, name))
// }

exports.exposeAff = (name, nargs) => {
  if (nargs == 0)
    return sbot => (fail, pass) => lookup(sbot, name)((err, data) => err ? fail(err) : pass(data))
  else if (nargs == 1)
    return sbot => a => (fail, pass) => lookup(sbot, name)(a, (err, data) => err ? fail(err) : pass(data))
  else if (nargs == 2)
    return sbot => a => b => (fail, pass) => lookup(sbot, name)(a, b, (err, data) => err ? fail(err) : pass(data))
  else
    throw "exposeAff not defined for many arguments"
}

exports.exposeEff = (name, nargs) => {
  if (nargs == 0)
    return sbot => () => lookup(sbot, name)()
  else if (nargs == 1)
    return sbot => a => () => lookup(sbot, name)(a)
  else if (nargs == 2)
    return sbot => a => b => () => lookup(sbot, name)(a, b)
  else
    throw "exposeEff not defined for many arguments"
}
