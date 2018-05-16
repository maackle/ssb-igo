const scuttlebot = require('scuttlebot')

const {exposeAff, exposeEff} = require('../Ssb.Common/foreign')

exports._sbotBuilder = plugins => () => {
  console.log("once")
  const r = (sbot, p) => sbot.use(p)
  const create = plugins.reduce(r, scuttlebot)
  return config => () => create(config)
}
exports._startSbot = scuttlebot
exports.requirePlugin = name => () => require(name)
exports.toPlugin = a => a

exports._createFeed = exposeEff(0, 'createFeed')
exports._createFeed1 = exposeEff(1, 'createFeed')
exports._createFeed2 = exposeEff(2, 'createFeed')
exports.use = exposeEff(1, 'use')

exports._whoami = exposeAff(0, 'whoami')
