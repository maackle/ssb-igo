const ssbClient = require('ssb-client')
const ssbKeys = require('ssb-keys')


exports._getClient = config => (error, success) => {
  config.caps = {
    shs: config.shs,
    sign: config.sign,
  }
  ssbClient(
    config.keys,
    config,
    (err, client) => {
      console.log('ccc', err, client)
      if (err) error(err)
      else success(client)
    }
  )
  // return (ce, cre, cs) => cs()
}

exports._publish = client => data =>
  (error, success) =>
    client.publish(data, (err, msg) => {
      console.log('publisheddd: ', err, msg)
      if (err) error(err)
      else success(msg)
    })


exports._close = client => {
  return (error, success) => {
    client.close(true)
    console.log("connection closed: ", client.closed)
    success()
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
