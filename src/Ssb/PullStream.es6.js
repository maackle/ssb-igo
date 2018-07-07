const pull = require("pull-stream")
const Abortable = require("pull-abortable")


exports._drainWith =
  stream => fn => (error, success) => {
    const op = d => fn(d)()
    const abortable = Abortable()
    const tag = Math.random();
    pull(
      stream,
      abortable,
      pull.drain(op, success)
    )
    return function (ce, cre, cs) {
      abortable.abort()
      cs()
    }
  }
