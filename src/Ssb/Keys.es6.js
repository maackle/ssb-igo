const ssbKeys = require('ssb-keys')

exports._loadOrCreateSync = filename => () => {
  return ssbKeys.loadOrCreateSync(filename)
}
