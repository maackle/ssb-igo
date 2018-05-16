
const ssbClient = require('ssb-client')
const ssbKeys = require('ssb-keys')

const path = "./ssb-data"
const keys = ssbKeys.loadOrCreateSync(path + "/secret")

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

ssbClient(keys, config, (err, sbot) => {
  const p = "./ssb-dev-bob/secret"
  sbot.ssbIgo.rawTestFeed(p, (err, f) => console.log(err, 'fff', f))
})
