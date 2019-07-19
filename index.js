const express = require('express')
const cmd = require('node-cmd')
const cors = require('cors')
const bodyParser = require('body-parser')
const app = express()
let processRef = cmd.get('cd MultiCategory && stack ghci')
app.use(cors())
app.use(bodyParser.text())
//"*Main GraphParser SQLParser SchemaCategory XMLParser>"

app.post('/query', (request, response) => {
    let stream = processRef.stdout
    stream.pipe(response)
    console.log(request.body)
    processRef.stdin.write(request.body + '\n')
    stream.on('data', (chunk) => {
        console.log(chunk)
        if(chunk.includes("*Main")) {
            response.end()
        }
      })
})

const PORT = 3002
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`)
})