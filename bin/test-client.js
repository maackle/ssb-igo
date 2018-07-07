
const ssbClient = require('ssb-client')
const ssbKeys = require('ssb-keys')

const path = "./ssb-data"
const keysMaster = ssbKeys.loadOrCreateSync(path + "/secret")
const keysAlice = ssbKeys.loadOrCreateSync(path + "/secret-alice")

const config = require('ssb-config/inject')('ssb', {
  path: path,
  // keys: keysMaster,
  host: "localhost",
  port: 8008,
  // master: keysMaster.id,
  // master: [keysMaster.id, keysAlice.id],
  caps: {
    shs: process.env.SBOT_SHS || "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=",
    sign: process.env.SBOT_SIGN || null,
  }
});

ssbClient(keysAlice, config, (err, sbot) => {
  console.log(err, sbot)
})
