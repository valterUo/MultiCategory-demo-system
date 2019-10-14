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

app.post('/relational', (request, response) => {
  fs.writeFile(request.get('FileName'), request.body, (err) => {
    console.log(err)
    if (processRef === undefined) {
      response.status(400).send({
        error: "The ML file has not been defined. The file was not uploaded."
      })
    } else {
      let stream = processRef.stdout
      processRef.stdin.write("val T'' = " + request.body + "\n")
      stream.once('data', function (data) {
        response.send("Relational data file created and the process is using the data.")
      })
    }
  })
})

app.post('/document', (request, response) => {
  fs.writeFile(request.get('FileName'), request.body, (err) => {
    console.log(err)
    if (processRef === undefined) {
      response.status(400).send({
        error: "The ML file has not been defined. The file was not uploaded."
      })
    } else {
      let stream = processRef.stdout
      processRef.stdin.write("val E'' = " + request.body + "\n")
      stream.once('data', function (data) {
        response.send("Document data file created and the process is using the data.")
      })
    }
  })
})

app.post('/graph', (request, response) => {
  fs.writeFile(request.get('FileName'), request.body, (err) => {
    console.log(err)
    if (processRef === undefined) {
      response.status(400).send({
        error: "The ML file has not been defined. The file was not uploaded."
      })
    } else {
      let stream = processRef.stdout
      processRef.stdin.write("val G'' = " + request.body + "\n")
      stream.once('data', function (data) {
        response.send("Graph data file created and the process is using the data.")
      })
    }
  })
})

const PORT = 3002
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`)
})