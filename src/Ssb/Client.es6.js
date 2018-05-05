const ssbClient = require('ssb-client')
const ssbKeys = require('ssb-keys')
const {exposeAff, exposeEff, exposePure} = require('../Ssb.Common/foreign')


exports.props = exposePure(0, null)


///////////////// SYNC

exports.unboxPrivate = exposeEff(2, ['private', 'unbox'])

//////////////// ASYNC


exports._close = client => {
  return (error, success) => {
    client.close(success)
  }
}

exports._getClient = config => (error, success) => {
  config.caps = {
    shs: config.shs,
    sign: config.sign,
  }
  ssbClient(
    config.keys,
    config,
    (err, client) => {
      if (err) error(err)
      else success(client)
    }
  )
  // return (ce, cre, cs) => cs()
}

exports._publish = client => data =>
  (error, success) =>
    client.publish(data, (err, msg) => {
      if (err) error(err)
      else success(msg)
    })

exports._publishPrivate = exposeAff(2, ['private', 'publish'])
exports._whoami = exposeAff(0, 'whoami')