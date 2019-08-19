# CATEGORY THEORY in MULTI-MODEL DATABASES

This project contains a category theoretical construction for demostrating how category theory can be applied in multi-model databases. This version is implemented with Haskell. The queries can be executed using [frontend](https://github.com/valterUo/demo-system-frontend) written with Javascript using React framework and D3.js visualization library.


## Installation

There are two ways to run this backend. Anyway, you need to have Stack installed: [Stack Tool for Haskell](https://docs.haskellstack.org/en/stable/README/).

### Run only Haskell program

After installing Stack, clone the project and navigate to the directory called Multicategory. The following instruction is more or less the conventional way to run Haskell programs with Stack in an interactive mode (REPL). In the directory you can build and run the project in an interactive mode with a command 

```
stack ghci
```

You can modify the Haskell source code and then reload the enviroment with a command

```
:reload
```

You can run the main function with a command 
```
:main
```
and you can exit the program with a command

```
:quit
```

### Start the server and Haskell program

After cloning the project, go to the directory demo-system-backend-Haskell and install the dependencies with a command

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
The latter one listens to changes in the program and restarts the server automatically after changes have been detected. Now server is open in the port 3002 and the frontend should be able to communicate with it.

## Usefull sites

You can find more about the theoretical backround of this program from the Overleaf document [Some Applications of Category Theory to Multi-Model Queries](https://www.overleaf.com/read/kqvkvrhcnmxv). I will write more to the document and update it time to time.

You might want to use Cabal instead of Stack: [Difference between Cabal and Stack](https://stackoverflow.com/questions/30913145/what-is-the-difference-between-cabal-and-stack).

## Future work

Future work includes expanding the data sets so that there are more data and it is more complicated way arranged.
