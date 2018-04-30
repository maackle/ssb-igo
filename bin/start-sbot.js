
const fs = require('fs')
const path = require('path')
const {ssbIgoPlugin} = require('../output/App.DB.Main')

function dumpManifest(sbot, filePath) {
  const manifest = JSON.stringify(sbot.getManifest())
  fs.writeFileSync(path.join(filePath, "manifest.json"), manifest)
}

function startSbot () {
  const path = "/Users/michael/.ssb-test"
  const keys = require('ssb-keys').loadOrCreateSync(path + "/secret")

  const config = require('ssb-config/inject')('ssb', {
    path: path,
    keys: keys,
    host: "localhost",
    port: 8088,
    master: keys.id,
    caps: {
      shs: process.env.SBOT_SHS || "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=",
      sign: process.env.SBOT_SIGN || null,
    }
  });

  sbot =
    require('scuttlebot')
    .use(require("scuttlebot/plugins/master"))
    .use(ssbIgoPlugin)
    (config)

  dumpManifest(sbot, path)

  console.info("sbot running")
}

startSbot()
