const express = require('express')
const neo4j = require('neo4j-driver').v1
const osmosis = require('osmosis')
const request = require('request')
const R = require('ramda')

const app = express()

const db = neo4j.driver('bolt://localhost', neo4j.auth.basic("neo4j", "neo4j"));

const knownTypes = [
  'KNOWN_NO',
  'KNOWN_YES',
  'KNOWN_VOID',
]
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

const getLinks = (url) => {
  let links = [], done = false;
  return new Promise((resolve, reject) => {
    osmosis
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
        .done(() => resolve(links))
        .log(console.log)
        .error(console.log)
        .debug(console.log)
  })
}

app.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});

app.get('/', function (req, res) {
  res.send('Hello World!')
})

app.get('/lookup/:term', (req, res) => {
  const {term} = req.params
  const url = `https://en.wikipedia.org/wiki/${term}`
  getLinks(url).then(links => {
    const query = `
      MERGE (c:Article {slug: {term} })
      FOREACH (
        d IN {links} |
        MERGE (a:Article {slug: d.slug, url: d.url})
        MERGE (c)-[:LINK {name: d.name}]->(a)
      )
    `
    return cypher(query, {term, links})
  }).then(() => {
    const query = `
      MATCH (c:Article {slug: {term}})
      MATCH (c)-[r]->(a)
      OPTIONAL MATCH (:User)-[k]->(a)
      RETURN a, r.name, type(k)
    `
    return cypher(query, {term})
  }).then(({records}) => {
    const subtree = records.map(rec => {
      return {
        slug: rec.get(0).properties.slug,
        url: rec.get(0).properties.url,
        name: rec.get(1),
        known: rec.get(2) || "KNOWN_VOID",
      }
    });
    res.send(JSON.stringify(subtree));
  })
})

app.get('/mark/:term/:level', (req, res) => {
  const userId = 11  // TODO
  const {term} = req.params
  const relationship = req.params.level
  const relationshipUnion = ':' + knownTypes.join('|:')
  if (!R.contains(relationship, knownTypes)) {
    res.send("invalid relationship type")
    return
  }
  const query = `
    MATCH (a:Article {slug: {term} })
    MATCH (u:User {id: {userId} })
    OPTIONAL MATCH (u)-[r${relationshipUnion}]->(a)
    DELETE r
    MERGE (u)-[:${relationship}]->(a)
    RETURN u
  `
  cypher(query, {term, userId}).then(r => {
    res.send("OK")
  })
})

app.listen(3000, function () {
  console.log('Example app listening on port 3000!')
})
