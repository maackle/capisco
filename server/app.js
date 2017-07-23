const express = require('express')
const neo4j = require('neo4j-driver').v1
const osmosis = require('osmosis')
const request = require('request')
const R = require('ramda')

const app = express()

const db = neo4j.driver('bolt://localhost', neo4j.auth.basic("neo4j", "neo4j"));

const cypher = (query, params) => {
  const session = db.session()
  return session.run(query, params)
    .then(res => {
      session.close()
      return res
    })
    .catch(err => {
      console.error('QUERY ERROR: ', err)
      session.close()
      return err
    })
}

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
    const query = `
      MERGE (c:Article {slug: {term} })
      FOREACH (
        d IN {links} |
        MERGE (a:Article {slug: d.slug})
        MERGE (c)-[:LINK {name: d.name}]->(a)
      )
    `
    cypher(query, {term, links}).then(() => {
      res.send(JSON.stringify(links))
    })
  })
})

app.get('/mark/:term/:level', (req, res) => {
  const userId = 11  // TODO
  const {term} = req.params
  const level = R.toLower(req.params.level)
  if (!R.contains(level, ['yes', 'no'])) {
    res.send("invalid relationship type")
    return
  }
  const relationship = level == 'yes' ? ':KNOWN_YES' : ':KNOWN_NO'
  const query = `
    MATCH (a:Article {slug: {term} })
    MATCH (u:User {id: {userId} })
    OPTIONAL MATCH (u)-[r:KNOWN_YES|:KNOWN_NO]->(a)
    DELETE r
    MERGE (u)-[${relationship}]->(a)
    RETURN u
  `
  cypher(query, {term, userId}).then(r => {
    console.log(r)
    res.send("OK")
  })
})

app.listen(3000, function () {
  console.log('Example app listening on port 3000!')
})
