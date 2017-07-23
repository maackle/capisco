const express = require('express')
const neo4j = require('neo4j-driver').v1
const osmosis = require('osmosis')
const request = require('request')

const app = express()

const db = neo4j.driver('bolt://localhost', neo4j.auth.basic("neo4j", "neo4j"));


const getLinks = (url, then) => {
  let links = [], done = false;
  return osmosis
    .get(url)
    .find('#bodyContent .mw-parser-output')
    .find('p > a, #toc > .toctitle')
    .set('name')
    .set({
      toctitle: 'h2',
      url: ['@href'],
    })
    .data(listing => {
      const {name, url} = listing
      if(listing.toctitle) {
        // only read links up to the table of contents
        done = true
      }
      if (!done && url.length == 1 && name) {
        const matches = url[0].match(/\/wiki\/(.+)/)
        if (matches) {
          links.push({
            name,
            url: url[0],
            slug: matches[1],
          })
        }
      }
    })
    .done(() => then(links))
    .log(console.log)
    .error(console.log)
    .debug(console.log)
}

app.get('/', function (req, res) {
  res.send('Hello World!')
})

app.get('/lookup/:term', (req, res) => {
  const {term} = req.params
  const url = `https://en.wikipedia.org/wiki/${term}`
  getLinks(url, links => {
    const session = db.session()
    links.map(link => {
      const {slug, name} = link
      const query = `
        MERGE (c:Article {slug: {term} })
        MERGE (a:Article {slug: {slug} })
        MERGE (c)-[:LINK {name: {name}}]->(a)
      `
      console.log('QQQQ', query)
      session.run(query, {term, slug, name}).then(result => {
        session.close()
        console.log('REZZ', result)
      }).catch((...args) => console.error("ERROR", args))
    })
    res.send(JSON.stringify(links))
  })
})

app.listen(3000, function () {
  console.log('Example app listening on port 3000!')
})
