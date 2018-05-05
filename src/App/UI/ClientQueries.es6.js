
const {exposeAff, exposeEff} = require('../Ssb.Common/foreign')

exports.getStream =
  client => () =>
    client.ssbIgo.streamDb()

exports._getDb =
  client => (fail, pass) => {
    return client.ssbIgo.rawGet((err, val) => err ? fail(err) : pass(val))
  }
