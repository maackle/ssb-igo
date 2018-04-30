const pull = require("pull-stream")
// exports.drain = x => y => console.log("xxx", x, "yyy", y)
//
exports._drain =
  stream => fn => (error, success) => {
    const op = d => {
      const ret = fn(d)()
      console.log('drainded', d, ret)
      return ret
    }
    const pipeline = pull(
      stream,
      pull.through(x => console.log("piping thru", x)),
      pull.drain(op, success)
    )
    return (ce, cre, cs) => cs()
  }
