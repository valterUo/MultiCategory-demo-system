# Multi-Model Query Translator

This project contains a simple program that translates the query language used in MultiCategory to Haskell. It does not work very well yet.

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
