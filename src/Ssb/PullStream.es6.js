const pull = require("pull-stream")

exports.drain = fn => (error, success) => {
  pull.drain(fn, success)
}
