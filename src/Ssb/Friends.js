var _require = require('purescript-ssb-util'),
    exposeAff = _require.exposeAff,
    exposeEff = _require.exposeEff;

exports._createFriendStream = exposeEff(1, 'friends.createFriendStream');
exports._aboutStream = exposeEff(0, 'about.stream');
exports._hops1 = exposeAff(1, 'friends.hops');
