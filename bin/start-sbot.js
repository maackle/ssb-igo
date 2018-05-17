
const fs = require('fs')
const path = require('path')
const ssbKeys = require('ssb-keys')
const {ssbIgoPlugin} = require('../output/App.DB.Main')

function dumpManifest(sbot, filePath) {
  const manifest = JSON.stringify(sbot.getManifest())
  fs.writeFileSync(path.join(filePath, "manifest.json"), manifest)
}

function startSbot () {
  const path = process.argv[2] || "./ssb-data"
  const port = process.argv[3] || 8088
  console.log(`starting sbot in ${path} at port ${port}`)
  const keysMaster = ssbKeys.loadOrCreateSync(path + "/secret")
  const keysAlice = ssbKeys.loadOrCreateSync(path + "/secret-alice")

  const config = require('ssb-config/inject')('ssb', {
    path: path,
    keys: keysMaster,
    host: "localhost",
    port: port,
    master: [keysMaster.id, keysAlice.id],  // doesn't actually work
    caps: {
      shs: process.env.SBOT_SHS || "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=",
      sign: process.env.SBOT_SIGN || null,
    }
  });

  sbot =
    require('scuttlebot')
    .use(require("scuttlebot/plugins/master"))
    .use(require("ssb-private"))
    .use(ssbIgoPlugin)
    (config)

  dumpManifest(sbot, path)

  console.info("sbot running")
}

startSbot()
