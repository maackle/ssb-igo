const electron = require('electron')
// Module to control application life.
const app = electron.app
// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow

const fs = require('fs')
const path = require('path')
const url = require('url')

const {ssbIgoPlugin} = require('../output/App.DB.Main')

require('electron-reload')(path.join(__dirname, 'ui'))

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow

let sbot

function createWindow () {
  // Create the browser window.
  mainWindow = new BrowserWindow({width: 800, height: 600})

  // and load the index.html of the app.
  mainWindow.loadURL(url.format({
    pathname: path.join(__dirname, 'index.html'),
    protocol: 'file:',
    slashes: true
  }))

  // Open the DevTools.
  // mainWindow.webContents.openDevTools()

  // Emitted when the window is closed.
  mainWindow.on('closed', function () {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null
  })
}

function dumpManifest(sbot, filePath) {
  const manifest = JSON.stringify(sbot.getManifest())
  fs.writeFileSync(path.join(filePath, "manifest.json"), manifest)
}

function startSbot () {
  const path = "/Users/michael/.ssb-test"
  const keys = require('ssb-keys').loadOrCreateSync(path + "/secret")

  const config = require('ssb-config/inject')('ssb', {
    path: path,
    keys: keys,
    host: "localhost",
    port: 8088,
    master: keys.id,
    caps: {
      shs: process.env.SBOT_SHS || "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=",
      sign: process.env.SBOT_SIGN || null,
    }
  });

  sbot =
    require('scuttlebot')
    .use(require("scuttlebot/plugins/master"))
    .use(ssbIgoPlugin)
    (config)

  dumpManifest(sbot, path)

  console.info("sbot running")
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', () => {createWindow(); startSbot()})

// Quit when all windows are closed.
app.on('window-all-closed', function () {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  console.info("closing sbot instance")
  sbot.close()
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

app.on('activate', function () {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow()
  }
})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.
