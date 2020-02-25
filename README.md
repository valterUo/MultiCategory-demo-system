# MultiCategory

## Category theory on multi-model databases

This project contains the category theoretical construction which demonstrates how category theory can be applied in multi-model databases. This version is implemented with Haskell. The queries can be executed using [frontend](https://github.com/valterUo/demo-system-frontend) written with React and D3.js.

## Installation

You need to have Stack installed: [Stack Tool for Haskell](https://docs.haskellstack.org/en/stable/README/) and Java version 11 or higher. Stack needs to be in PATH. At the moment MultiCategory runs only on Windows. You can download a zip-file from the [release page](https://github.com/valterUo/MultiCategory-demo-system/releases) of this repository. The file contains all necessary libraries and example data to run MultiCategory.

### Install first Haskell part of the backend

Install first the Haskell part of the program. Unzip file that you loaded from the release page of this Github repository and navigate to the folder called MultiCategory. This folder contains a source code for the Haskell part of the program. Start the Haskell program by calling stack: 

```
stack ghci
```

First all the necessary packages are downloaded and after that the program is compiled. The first run takes some time especially if the stack tool has not been used before. If the execution fails, run the command stack ghci again. When the execution is successfully finished, you should have REPL open in the console. You can now close the console.

### Start the Java Spring server

Navigate to the folder which contains MultiCategory-backend-0.0.1-SNAPSHOT.jar file and execute the command

```
java -jar MultiCategory-backend-0.0.1-SNAPSHOT.jar
```
This starts the server. After around ten seconds you can open a browser and open the page http://localhost:8080/. Now you see the graphical interface of MultiCategory and you can study the demo. Enjoy!

## Guide to read code in this repository and its connections to category theory

MultiCategory is coded as an ordinary fullstack program with Haskell integration. It consists of the frontend and the backend. The frontend is written with React.js and D3.js frameworks. It is reponsible of user interaction and data visualization. 

The backend can be divided into two parts: the first part is a Java program which exploits Spring Framework. It creates a server, it offers the static build version of the frontend, it is responsible of query tokenizing, parsing and translating and it starts the Haskell program. The Haskell program is another part of the backend and it contains the category theoretical implementations and data stuctures that our demo is using.

### Frontend

You can find the build version of frontend in the src/main/resources/static folder. The frontend is based on the source code from [frontend](https://github.com/valterUo/demo-system-frontend).

### Java program

#### Query tokenizing, parsing and translating

Each selective query becomes and instance of a class called [SelectiveQuery](https://github.com/valterUo/MultiCategory-demo-system/blob/master/src/query/SelectiveQuery.java). When SelectiveQuery object is created, the input query string is automatically tokenized, parsed and translated so that user can output the corresponding Haskell code i.e. the sequence of fold functions. Each SelectiveQuery consists of one or multiple [QueryBlocks](https://github.com/valterUo/MultiCategory-demo-system/blob/master/src/query/QueryBlock.java) which have one or multiple [LambdaFunctions](https://github.com/valterUo/MultiCategory-demo-system/blob/master/src/query/LambdaFunction.java).

This structure of classes creates a simple parse tree of the query and it can be efficiently used to modify and understand the query. Certain keywords (cons, nil) are modified with respect to the target model of the query and the correct fold function is used with respect to the target model of the query. The [CodeGenerator](https://github.com/valterUo/MultiCategory-demo-system/blob/master/src/codeGenerator/CodeGenerator.java) class is partly responsible of this.

#### Running the Haskell program

Classes in the [process](https://github.com/valterUo/MultiCategory-demo-system/tree/master/src/process) folder are responsible of handeling the process that runs the Haskell program. Translated queries are feeded to the process and the program returns the results or the corresponding error messages. The result is written to a file where it is parsed and stored in-memory JSONDB database. The executed queries are also stored into this database. The frontend can request the executed queries and their results from the backend.

### Haskell program

The Haskell program is stored in the folder called [MultiCategory](https://github.com/valterUo/MultiCategory-demo-system/tree/master/MultiCategory) and the user can use it independently.

#### Schema and Instance category implementations

 The schema categories are implemented as modules that contain Haskell datatypes. These schema categories can be found from the folder [src](https://github.com/valterUo/MultiCategory-demo-system/tree/master/MultiCategory/src) which contains folders named after the data sets. Each folder contains a file called SchemaCategory.hs. Each of these folders contains also a file called DataState.hs which contains the objects that are mapped with collection constructor functors from the schema category. The morphisms are not implemented explicitly since we consider that all the Haskell functions (except the undefined) to be morphisms.

#### Data stuctures

The Haskell program uses multiple different data stuctures. These include [lists](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html), [algebraic graphs](http://hackage.haskell.org/package/algebraic-graphs-0.4/docs/Algebra-Graph.html), [Data.IntMap.Strict](http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-IntMap-Strict.html), [Data.HashMap.Strict](https://hackage.haskell.org/package/unordered-containers-0.2.10.0/docs/Data-HashMap-Strict.html), [NimbleGraph](https://github.com/valterUo/MultiCategory-demo-system/tree/master/MultiCategory/src/NimbleGraph) and [RDF graphs](http://hackage.haskell.org/package/rdf4h-3.1.0).

The data is parsed from the files (for example the [simple demo data](https://github.com/valterUo/MultiCategory-demo-system/tree/master/MultiCategory/demoData)) using [xeno](https://hackage.haskell.org/package/xeno) for XML parsing, [aeson](http://hackage.haskell.org/package/aeson) for JSON encoding and decoding and [rdf4h](http://hackage.haskell.org/package/rdf4h-3.1.0) for RDF parsing. The Haskell program implements its own simple module for CSV parsing. When we consider graphs, the program implements also functions for transforming graphs (algebraic graphs, RDF graphs and NimbleGraphs) into format that D3.js accepts in the frontend side.

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

The user does not need to know the certain cons and nil functions for the colletions but the system knows them based on the data sets and it knows how to use them with different combinations. Now the user is also allowed to input multiple lambda functions. If the user does not provide a lambda function, then certain initial settings are used.

Unfortunately, the system cannot know how user wants to build richer structures from primitive structures. For example, if the user is querying a table to graph, they need to define explicitly how data is organized in the output graph. On the otherhand, if the user is querying graph to table, then it suffices to use cons and nil functions and multiple lambda functions.