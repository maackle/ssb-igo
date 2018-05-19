const pull = require("pull-stream")
const Abortable = require("pull-abortable")


exports._drainWith =
  stream => fn => (error, success) => {
    const op = d => fn(d)()
    const abortable = Abortable()
    pull(
      stream,
      abortable,
      pull.through(x => console.log("piping thru", x)),
      pull.drain(op, success)
    )
    return function (ce, cre, cs) {
      console.info('aborting drain')
      abortable.abort()
      cs()
    }
  }
