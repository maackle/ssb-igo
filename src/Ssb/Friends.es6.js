
const {exposeAff, exposeEff} = require('purescript-ssb-util')

exports._createFriendStream = exposeEff(1, 'friends.createFriendStream')
exports._aboutStream = exposeEff(0, 'about.stream')
exports._hops1 = exposeAff(1, 'friends.hops')
