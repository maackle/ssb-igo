
exports.getStream =
  client => () =>
    client.ssbIgo.streamDb()
