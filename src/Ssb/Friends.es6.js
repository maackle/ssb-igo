
const {exposeAff, exposeEff} = require('../Ssb.Common/foreign')

exports._createFriendStream = exposeEff(1, 'friends.createFriendStream')
exports._hops1 = exposeAff(1, 'friends.hops')
