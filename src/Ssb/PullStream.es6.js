const pull = require("pull-stream")
const Abortable = require("pull-abortable")


exports._drainWith =
  stream => fn => (error, success) => {
    const op = d => fn(d)()
    const abortable = Abortable()
    const tag = Math.random();
    console.log('listen to ', tag)
    pull(
      stream,
      abortable,
      pull.through(x => console.log("piping thru", tag, x)),
      pull.drain(op, success)
    )
    return function (ce, cre, cs) {
      console.info('aborting drain')
      abortable.abort()
      cs()
    }
  }
