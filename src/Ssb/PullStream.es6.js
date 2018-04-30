const pull = require("pull-stream")
// exports.drain = x => y => console.log("xxx", x, "yyy", y)
//
exports._drain =
  stream => fn => (error, success) => {
    pull(stream, pull.drain(d => fn(d)(), success))
    return (ce, cre, cs) => cs()
  }
