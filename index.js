const express = require('express')
const cmd = require('node-cmd')
const cors = require('cors')
const bodyParser = require('body-parser')
const fs = require('fs')
const app = express()
let processRef = cmd.get('cd MultiCategory && stack ghci')
app.use(cors())
app.use(bodyParser.text({limit: '200mb'}))
app.use(express.static('build'))

app.post('/query', (request, response) => {
    let stream = processRef.stdout
    stream.pipe(response)
    processRef.stdin.write(request.body + '\n')
    stream.on('data', (chunk) => {
      console.log("On data...")
      if(chunk.includes("*Main")) {
          stream.pause()
          stream.removeAllListeners()
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
      let fileNames = request.get('FileNames').split(";")
      let filePath = fileNames.reduce((total, current) => total = total + ' \"uploadedFiles\\\\' + current + '\"', "")
      let command = request.get('VariableName') + " = unsafePerformIO $ " + request.get('UploadingFunction') + filePath
      processRef.stdin.write(command + "\n", () => {console.log("Data uploaded.")})
      processRef.stdin.write("\n")
      //processRef.stdin.removeAllListeners()
    }
  })
  response.end("File has been uploaded and the backend is using the data.")
})

app.post('/uploadWithoutUse', (request, response) => {
  fs.writeFile("./MultiCategory/uploadedFiles/" + request.get('FileName'), request.body, (err) => {
    console.log(err)
    if (processRef === undefined) {
      response.status(400).send({
        error: "Error! The file has not been uploaded."
      })
    } else {
      response.end("File has been uploaded.")
    }})
})

const PORT = 3002
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`)
})