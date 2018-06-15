const tenuki = require('tenuki')

const unwrapEff = cb => (...args) => cb(...args)()

exports._createGame = element => terms => () => {
  const game = new tenuki.Game(Object.assign({element}, terms))
  return game
}

exports._createClient = element => terms => player => callbacks => () => {
  const hooks = {
    submitPlay: (y, x, cb) => {
      callbacks.submitPlay({x,y})()
      cb(true)
    }
  }

  const client = new tenuki.Client(
    Object.assign({element, player, hooks}, terms)
  )
  return client
}

exports.setMoveCallback = game => cb => () => {
  game.callbacks.postMove = unwrapEff(cb)
}

exports.getGame = client => client._game
exports.currentState = game => game.currentState()

exports.playPass = opts => game => () => game.playPass(opts)
exports.playResign = opts => game => () => game.playResign(opts)
exports.playAt = opts => ({x, y}) => game => () => game.playAt(y, x, opts)
exports.render = game => () => game.render()
