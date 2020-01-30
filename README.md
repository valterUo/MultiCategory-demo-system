# MultiCategory

## Category theory on multi-model databases

This project contains the category theoretical construction which demonstrates how category theory can be applied in multi-model databases. This version is implemented with Haskell. The queries can be executed using [frontend](https://github.com/valterUo/demo-system-frontend) written with React and D3.js.

## Installation

You need to have Stack installed: [Stack Tool for Haskell](https://docs.haskellstack.org/en/stable/README/) and [Node.js](https://nodejs.org/en/) for npm package manager. After the installation the demo system open to development mode.

### Install first Haskell part of the backend

Installing first the Haskell part of the program is the best. Clone the github project to some directory i.e. run the following code in a console in the directory where you want to clone the project:

```
git clone https://github.com/valterUo/demo-system-backend-Haskell.git
```
Then navigate to the folder that contains the Haskell part of the backend by using the following command. The folder is called MultiCategory.

```
cd demo-system-backend-Haskell\MultiCategory
```

Start the Haskell program by calling stack. Stack needs to be in PATH.

```
stack ghci
```

First all the necessary packages are downloaded and after that the program is compiled. The first run takes some time especially if the stack tool has not been used before. If the execution fails, run the command stack ghci again. When the execution is successfully finished, you should have REPL open in the console. You can now close the console.

### Start Node.js and Haskell program together

After installing the Haskell part of the program, go to the directory demo-system-backend-Haskell (Do not run this in MultiCategory folder) and install the Node.js dependencies with a command

```
npm install
```

Then you can start the server by command
```
npm start
```
or
```
npm run watch
```
The latter one listens to changes in the program and restarts the server automatically after changes have been detected. Now the demo system is running and you can access it from a browser by going to http://localhost:3002/. The backend is offering the static frontend page automatically.

If you run npm commands before installing the Haskell part of the program, stack tries to install all the necessary dependencies in the background and that will take long! 

### Run only Haskell program

You can open only the Haskell part of the program with the command

```
stack ghci
```
Then the program is opened to REPL and you can execute Haskell code. The frontend offers ''Query corresponds fold'' box where the parsed query is printed. You can run this parsed query also by copy-pasting it to the Haskell program.

You can modify the Haskell source code and then reload the environment with a command

```
:reload
```

You can call functions like
```
foldr (\x xs -> x:xs) [] locations
```
and you can exit the program with a command

```
:quit
```

For debugging purposes it is useful to start the Haskell program alone since error messages in the frontend are not always describing.
