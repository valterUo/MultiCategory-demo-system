# MultiCategory

## Category theory on multi-model databases

This project contains the category theoretical construction which demonstrates how category theory can be applied in multi-model databases. This version is implemented with Haskell. The queries can be executed using [frontend](https://github.com/valterUo/demo-system-frontend) written with React and D3.js.

## Installation

You need to have Stack installed: [Stack Tool for Haskell](https://docs.haskellstack.org/en/stable/README/) and Java version 11 or higher. 

In the future there will be a jar file that creates a server.

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


## Generally

This project contains a simple program that translates the query language used in MultiCategory to Haskell. See the test file for more examples.

For example:

```
LET t BE 
QUERY (\x -> if customerName x == "Alice" then cons x else nil) 
FROM customers
TO relational 
IN
QUERY (\x -> if any (\y -> knows x y customers) t then cons x else nil)
FROM customers
TO algebraic graph
```

becomes a sequence of folds:

```
let t = 
foldg  [] (\x -> if customerName x == "Alice" then [x]  else [] ) (\x y -> union x y ) (\x y -> union x y )  customers
in
foldg  Algebra.Graph.empty (\x -> if any ( \y -> knows x y customers ) t then Vertex x else Algebra.Graph.empty ) (\x y -> overlay x y ) (\x y -> connect x y )  customers.
```

The user does not need to know the certain cons and nil functions for the colletions but the system knows them based on the data sets and it knows how to use them with different combinations. Now the user is also allowed to input multiple lambda functions.

Unfortunately, the system cannot know how user wants to build richer structures from primitive structures. For example, if the user is querying a table to graph, they need to define explicitly how data is organized in the output graph. On the otherhand, if the user is querying graph to table, then it suffices to use cons and nil functions and multiple lambda functions.

## About Java program

Each selective query becomes and instance of a class called SelectiveQuery. When SelectiveQuery object is created, the input query string is automatically tokenized, parsed and translated so that user can output the correspongind Haskell code i.e. the sequence of fold functions.

Each SelectiveQuery has one or many QueryBlock objects. Each QueryBlock has target and domain model information and the domain data set information. Besides, every QueryBlock has one or many LambdaFunction objects. This structure creates a parse tree of the query and it can be efficiently used to modify and understand the query.
