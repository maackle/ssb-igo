var _require = require('./ssb-common'),
    exposeAff = _require.exposeAff,
    exposeEff = _require.exposeEff;

exports.getStream = function (client) {
  return function () {
    return client.ssbIgo.streamDb();
  };
};

exports._getDb = function (client) {
  return function (fail, pass) {
    return client.ssbIgo.rawGet(function (err, val) {
      return err ? fail(err) : pass(val);
    });
  };
};

// exports._testFeed =
//   client => path => (fail, pass) =>
//     client.ssbIgo.rawTestFeed(path, (err, val) => err ? fail(err) : pass(val))
