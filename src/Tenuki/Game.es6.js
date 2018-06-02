const tenuki = require('tenuki')

exports.createGame = element => () => {
  const game = new tenuki.Game({element})
  console.log('TENUKI SETUP', element, game)
  return game
}

exports.playPass = opts => game => () => game.playPass(opts)
exports.playResign = opts => game => () => game.playResign(opts)
exports.playAt = opts => ({x, y}) => game => () => game.playAt(y, x, opts)
exports.render = game => () => game.render()
