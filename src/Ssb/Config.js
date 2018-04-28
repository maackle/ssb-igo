var ssbKeys = require('ssb-keys')

exports._defaultConfig = function (path) {
  return function () {
    if (!path) throw Error("TODO: Handle no path")
    var keys = ssbKeys.loadOrCreateSync(path + "/secret");
    return {
      keys: keys,
      path: path,
      shs: "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=", // TODO change
      sign: null,
      host: "localhost",
      port: 8008,
    }
  }
}
