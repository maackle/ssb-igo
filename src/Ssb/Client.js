'use strict';

var ssbClient = require('ssb-client');
var ssbKeys = require('ssb-keys');

exports._getClient = function (config) {
  return function (error, success) {
    config.caps = {
      shs: config.shs,
      sign: config.sign };
    ssbClient(config.keys, config, function (err, client) {
      console.log('ccc', err, client);
      if (err) error(err);else success(client);
    });
  };
};

exports._publish = function (client) {
  return function (data) {
    return function (error, success) {
      return client.publish(data, function (err, msg) {
        console.log('published: ', err, msg);
        if (err) error(err);else success(msg);
      });
    };
  };
};

exports._close = function (client) {
  return function (error, success) {
    client.close(true);
    console.log('connection closed: ', client.closed);
    success();
  };
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
; // return (ce, cre, cs) => cs()
