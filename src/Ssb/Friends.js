'use strict';

var _require = require('../../output/Ssb.Common/foreign');

var exposeAff = _require.exposeAff;
var exposeEff = _require.exposeEff;

exports._createFriendStream = exposeEff(1, 'friends.createFriendStream');
exports._hops1 = exposeAff(1, 'friends.hops');
