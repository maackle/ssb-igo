const tenuki = require('tenuki')

exports._createGame = element => terms => () => {
  const game = new tenuki.Game(Object.assign({element}, terms))
  return game
}

exports.setMoveCallback = game => cb => () => {
  game.callbacks.postMove = (...args) => cb(...args)()
}

exports.playPass = opts => game => () => game.playPass(opts)
exports.playResign = opts => game => () => game.playResign(opts)
exports.playAt = opts => ({x, y}) => game => () => game.playAt(y, x, opts)
exports.render = game => () => game.render()
