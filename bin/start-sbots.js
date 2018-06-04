
const {fork} = require('child_process')

devs = ['alice', 'bob', 'charlie']
sbots = {}
forks = {}

devs.forEach((name, i) => {
  port = 8080 + i + 1
  forks[name] = fork(`bin/start-sbot-child.js`, [name, port])
  forks[name].on('message', msg => {
    sbots[msg.name] = msg
    if (process.argv[2] && Object.keys(sbots).length == devs.length) {
      console.log("Setting up follow graph")
      // carry on
      devs.forEach(dev1 => {
        devs.forEach(dev2 => {
          if (dev1 !== dev2) {
            const fork1 = forks[dev1]
            const sbot1 = sbots[dev1]
            const fork2 = forks[dev2]
            const sbot2 = sbots[dev2]
            console.log(`${sbot1.name} => ${sbot2.name}`)
            console.log(`${sbot1.id} => ${sbot2.id}`)
            fork1.send({
              gossip: {
                host: 'localhost',
                port: sbot2.port,
                key: sbot2.id,
              },
              contact: {
                type: 'contact',
                contact: sbot2.id,
                following: true,
              }
            })
          }
        })
      })
    }
  })

})
