var _require = require('./ssb-common'),
    exposeAff = _require.exposeAff,
    exposeEff = _require.exposeEff;

exports._createFriendStream = exposeEff(1, 'friends.createFriendStream');
exports._hops1 = exposeAff(1, 'friends.hops');
