var ssbClient = require('ssb-client')
var ssbKeys = require('ssb-keys')

exports._getClient = function (error, success) {
    console.log("getClient outer")
    var path = "/Users/michael/.ssb-test"
    var config = {
      keys: ssbKeys.loadOrCreateSync(path + "/secret"),
      path: path,
      caps: {shs: "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A="},
      host: "localhost"
    }
    console.log(config)
    ssbClient(
      config.keys,
      config,
      function (err, client) {
        console.log("getClient inner")
        if (err) error(err)
        else success(client)
      })
    return function (ce, cre, cs) {
      cs()
    }
  }

exports._publish = function (client, data) {
  console.log("publish outer")
  return function (error, success) {
    client.publish(data, function(err, msg) {
      console.log("publish inner")
      if (err) error(err)
      else success(msg)
    })
    return function (ce, cre, cs) {
      cs()
    }
  }
}
//
// exports._getClient2 = function () {
// console.log("getClient outer outer OUTER")
//   return function () {
//   console.log("getClient outer outer")
//   return function (error, success) {
//     console.log("getClient outer")
//     ssbClient("/Users/michael/.ssb-test/secret", function (err, client) {
//       console.log("getClient inner")
//       if (err) error(err)
//       else success(client)
//     })
//     return function () { return function (ce, cre, cs) {
//       cs()
//     }}
//   }
// }}
//
// exports._publish2 = function () {
//   console.log("publish OUT OUT OUT")
//   return function (client, data) {
//   console.log("publish outer")
//   return function (error, success) {
//     client.publish(data, function(err, msg) {
//       console.log("publish inner")
//       if (err) error(err)
//       else success(msg)
//     })
//     return function () { return function (ce, cre, cs) {
//       cs()
//     }}
//   }
// }}
