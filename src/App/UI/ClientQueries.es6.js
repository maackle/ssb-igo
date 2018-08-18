
const {exposeAff, exposeEff} = require('purescript-ssb-util')

exports.getStream =
  client => () =>
    client.ssbIgo.streamDb()

exports._getDb =
  client => (fail, pass) => {
    return client.ssbIgo.rawGet((err, val) => err ? fail(err) : pass(val))
  }

// exports._testFeed =
//   client => path => (fail, pass) =>
//     client.ssbIgo.rawTestFeed(path, (err, val) => err ? fail(err) : pass(val))
