# CATEGORY THEORY in MULTI-MODEL DATABASES

This project contains a category theoretical construction for demostrating how category theory can be applied in multi-model databases. This version is implemented with Haskell. The another version implemented with Standart ML (and little bit Node.js) is [here](https://github.com/valterUo/demo-system-backend). Both of these versions can be operated with the [frontend](https://github.com/valterUo/demo-system-frontend) written with Javascript using React framework and D3.js visualization library.

## How does it look like?

1. There exists three different parsers related to relational, tree and graph types of data. Table-like data is assumed to input as .csv file each table in a separete file. Tree model data is assumed to be in .xml format. Graph data is assumed to be a list of pairs.

2. There is a strict schema for this category theoretical construction.

3. There is a module that is responsible of creating an interface that communicates with the [frontend](https://github.com/valterUo/demo-system-frontend).

## Installation

There are two ways to run this backend.

### Run only Haskell program

You need to have Stack installed: [Stack Tool for Haskell](https://docs.haskellstack.org/en/stable/README/). After installing Stack, clone the project and navigate to the right directory that is called Multicategory. The following instruction is more or less the conventional way to run Haskell programs with Stack in an interactive mode (REPL). In the directory you can build and run the project in an interactive mode with a command 

```
stack ghci
```
Now you are ready to execute queries in the command line. For example, you can try the following queries:

```
evaluatePredicate customers (\y -> True)
evaluatePredicate customers (\y -> customerId y == 6) $$ ((\x y -> knows y x), customers) ^=^ (\x -> creditLimit x > 1000)
evaluatePredicate orders (\y -> orderNumber y == "3qqqeq9") $$ ((\y x -> contains y x), products) ^=^ (\x -> productPrice x > 50)
evaluatePredicate orders (\x -> elem "Carpet" (map productName (orderProducts x))) $$ ((\x y -> ordered x customers == y), customers)
evaluatePredicate customers (\y -> customerName y == "Alice") $$ ((\x y -> knows x y), customers)
evaluatePredicate customers (\y -> customerName y == "Alice") $$ ((\x y -> knows x y), customers) $$ ((\x y -> knows x y), customers) $$ ((\x y -> ordered y customers == x), orders) ^=^ (\x -> (sum $ map productPrice (orderProducts x)) > 5000)
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
