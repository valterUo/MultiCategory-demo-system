const express = require('express')
const cmd = require('node-cmd')
const cors = require('cors')
const bodyParser = require('body-parser')
const fs = require('fs')
const app = express()
let processRef = cmd.get('cd MultiCategory && stack ghci')
app.use(cors())
app.use(bodyParser.text({limit: '200mb'}))

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

app.post('/fileUpload', (request, response) => {
  fs.writeFile("./MultiCategory/uploadedFiles/" + request.get('FileName'), request.body, (err) => {
    console.log(err)
    if (processRef === undefined) {
      response.status(400).send({
        error: "Error! The file has not been uploaded."
      })
    } else {
      let stream = processRef.stdout
      let command = request.get('VariableName') + " = unsafePerformIO $ " + request.get('UploadingFunction') + ' \"uploadedFiles\\\\' + request.get('FileName') + '\"' + '\n'
      console.log(command)
      processRef.stdin.write(command)
      stream.on('data', (chunk) => {
        console.log(chunk)
        if(chunk.includes("*Main")) {
            response.end("File has been uploaded and the backend is using the data.")
        }
      })
    }
  })
})

const PORT = 3002
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`)
})