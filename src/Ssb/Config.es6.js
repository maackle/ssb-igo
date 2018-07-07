const home = require('os-homedir')
const ssbKeys = require('ssb-keys')

exports._defaultConfig = path => keys => () => {
  if (!path) path = home()
  if (!path) {
    path = 'browser'
  } else {
    path = path + '/.ssb'
  }
  if (!keys) keys = ssbKeys.loadOrCreateSync(path + "/secret");
  return {
    keys: keys,
    path: path,
    // shs: "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=", // TODO change
    sign: null,
    host: "localhost",
    port: 8008,
    manifest: null,
  }
}
