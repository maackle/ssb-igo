
const fs = require('fs')
const path = require('path')
const pull = require('pull-stream')
const ssbKeys = require('ssb-keys')
const bundle = require('../dist/db.js')
const {ssbIgoPlugin} = bundle

console.log(ssbIgoPlugin)

const sbotBuilder =
  require('scuttlebot')
  .use(require("scuttlebot/plugins/master"))
  .use(require("scuttlebot/plugins/gossip"))
  .use(require("scuttlebot/plugins/replicate"))
  .use(require("ssb-private"))
  .use(require("ssb-friends"))
  .use(ssbIgoPlugin)

function dumpManifest(sbot, filePath) {
  const manifest = JSON.stringify(sbot.getManifest())
  fs.writeFileSync(path.join(filePath, "manifest.json"), manifest)
}

exports.startSbot = (port) => {

  const config = require('ssb-config/inject')('ssb', {
    host: "localhost",
    port: port,
  });

  config.keys = ssbKeys.loadOrCreateSync(config.path + "/secret")
  config.master = config.keys.id
  console.log(`starting sbot in ${config.path} at port ${port}`)

  const sbot = sbotBuilder(config)
  dumpManifest(sbot, config.path)

  return sbot
}
