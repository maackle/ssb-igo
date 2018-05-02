const ssbClient = require('ssb-client')
const ssbKeys = require('ssb-keys')
const {exposeAff, exposeEff, exposePure} = require('../Ssb.Common/foreign')

exports._close = client => {
  return (error, success) => {
    client.close(true)
    console.log("connection closed: ", client.closed)
    success()
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
      console.log('publisheddd: ', err, msg)
      if (err) error(err)
      else success(msg)
    })

exports._publishPrivate = exposeAff(['private', 'publish'], 2)
exports._unboxPrivate = exposeEff(['private', 'unbox'], 1)
exports._whoami = exposeAff('whoami', 0)
