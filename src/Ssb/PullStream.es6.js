const pull = require("pull-stream")
// exports.drain = x => y => console.log("xxx", x, "yyy", y)
//
exports._drain =
  stream => fn => (error, success) => {
    const op = d => fn(d)()
    pull(
      stream,
      pull.through(x => console.log("piping thru", x)),
      pull.drain(op, success)
    )
    return (ce, cre, cs) => cs()
  }
