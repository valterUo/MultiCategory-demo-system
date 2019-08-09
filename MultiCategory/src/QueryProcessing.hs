module QueryProcessing where

import Data.List
import Algebra.Graph
import SchemaCategory

-- Another version of simple demo data:
-- Graph
customers2 = edges [(Customer 1 "John" 2000 10, Customer 6 "Mill" 0 11), 
                        (Customer 3 "Alice" 200 12, Customer 6 "Mill" 0 11), 
                        (Customer 6 "Mill" 0 11, Customer 3 "Alice" 200 12), 
                        (Customer 3 "Alice" 200 12, Customer 1 "John" 2000 10), 
                        (Customer 1 "John" 2000 10, Customer 2 "William" 3000 13), 
                        (Customer 0 "Mary" 5000 14, Customer 5 "Erica" 8000 16), 
                        (Customer 4 "William" 30 15, Customer 2 "William" 3000 13), 
                        (Customer 4 "William" 30 15, Customer 5 "Erica" 8000 16), 
                        (Customer 0 "Mary" 5000 14, Customer 0 "Mary" 5000 14), 
                        (Customer 1 "John" 2000 10, Customer 1 "John" 2000 10), 
                        (Customer 2 "William" 3000 13, Customer 2 "William" 3000 13), 
                        (Customer 3 "Alice" 200 12, Customer 3 "Alice" 200 12), 
                        (Customer 4 "William" 30 15, Customer 4 "William" 30 15), 
                        (Customer 5 "Erica" 8000 16, Customer 5 "Erica" 8000 16), 
                        (Customer 6 "Mill" 0 11, Customer 6 "Mill" 0 11), 
                        (Customer 7 "Bob" 9999 10, Customer 7 "Bob" 9999 10)]

-- Relational data:
locations = [Location 10 "Pont du Faubourg, N89" "La Roche-en-Ardenne" 6980 "Belgium", Location 11 "Lietaus g. 51" "Vilnius" 04231 "Lithuania", Location 12 "Masterton Castlepoint Road" "Tinui" 5889 "New Zealand", Location 13 "535 Pasir Ris Drive 1" "Northeast" 510535 "Singapore", Location 14 "N 2" "Sandweiler" 5238 "Luxembourg", Location 15 "Avenida Adolfo Eastman" "Olmue" 2330505 "Chile", Location 16 "Industrivej 5" "Kjellerup" 8620 "Denmark"]

-- The plan is write a such "query language" based on Haskell that all the possible subsets of the data (whatever it means when you take a subset of a structured construction) can be queried. Obviously, this includes lot of different combinations: 

-- – simple filtering results. This means that if we have a graph, we just filter this graph smaller e.g. we return a subgraph. If we have a table we just return a subtable e.g. part of the rows. If we have a tree, we return just a subtree. 
--     All the attributes i.e. columns and vertices are as they are in the original data before query execution.

-- – We can have filtered results with another filtering condition so that we do not choose all the attributes from the nodes or rows into the result. For example, this means that we filter columns and rows of a table.

-- – With respect to the morphisms in the schema category certains joins can be done:
-- 	– If two tables have key-foreign key pairs then this becomes a morphism between two tables and we can join the rows in a result.
-- 	– If, for example, customer has made an order, then we have a morphism Order -> Customer which is between XML and graph data. Thus we can make a join where we use this morphism to join this data so that we obtain a new graph 	where each node contains customer and order, just orders or we can create a tree that contains customers.

-- – Graph queries: pattern matching: create a pattern (possibly a graph) so that each vertex contains some filtering condition, for example, Filter f = FilterFunction f | Id f, Graph Int = Overlay (Vertex 1) (Connect (Vertex 2) (Vertex 3))
-- Stupid algorithm for this would be substitute all the possible combinations of vertices 1,2 and 3 to the pattern from the original target graph (those vertices that satisfy the filtering condition) and then check if this is a subgraph of the target graph.

-- foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b

connectViaMorphism :: (Foldable t, Eq b) => a -> (a -> b) -> (c -> b) -> t c -> c
connectViaMorphism x f g structure = head $ foldr (\y ys -> if g y == f x then y:ys else ys) [] structure

--connectStructuresViaMorphism :: (Foldable t, Eq b, Monoid m) => a -> (a -> a -> a)
--connectStructuresViaMorphism init cons structure1 f g structure2 = foldr (\y ys -> let t = connectViaMorphism y f g structure1 in cons t ys else ys) init structure2

connectStructureToGraph :: (Foldable t, Eq b) => Graph a -> (a -> b) -> (c -> b) -> t c -> Graph (a, c)
connectStructureToGraph initialGraph f g connectingStructure = foldg empty (\x -> let w = connectViaMorphism x f g connectingStructure in vertex (x, w)) overlay connect initialGraph

execute :: Foldable t => (a -> b -> b) -> b -> t a -> b
execute f i d = foldr f i d

-- In graph we assume that all the data is stored in nodes (edges cannot be modified at this time)
-- For filtering nodes, there already exists induce :: (a -> Bool) -> Graph a -> Graph a
-- Visits every vertex, returns a graph

foldVertices :: (a -> b) -> Graph a -> Graph b
foldVertices f graph = foldg empty (\x -> vertex $ f x) overlay connect graph

join :: Foldable t => (a -> b -> b) -> b -> t a -> b
join t y u = undefined

------------------------------------------------------------------------------------------------------------------------
-- This is the older not complite implementation.
-- Evaluation of the predicate should work as functor for fixed predicate f. 
-- We input a list of elements that are instances of a datatype, then a predicate and the function return a list that contains those elements for which the predicate is true.
-- If the answer is empty, we return Nothing.

evaluatePredicate :: [a] -> (a -> Bool) -> [a]
evaluatePredicate [] _ = []
evaluatePredicate (x:xs) f = if f x then let ys = evaluatePredicate xs f in x:ys else evaluatePredicate xs f 

-- We can compose evaluatePredicate functions' answers so that the answer is the new initial set for evaluatePredicate. Compare to Monad structure in Haskell.

(^=^) :: [a] -> (a -> Bool) -> [a]
[] ^=^ _ = []
xs ^=^ b = evaluatePredicate xs b

-- This is a helper function for the operator $$.

evaluateDouble :: (a -> b -> Bool) -> a -> [b] -> [b]
evaluateDouble _ _ [] = []
evaluateDouble f x (y:ys) = if f x y then y:(evaluateDouble f x ys) else evaluateDouble f x ys

($$) :: [a] -> ((a -> b -> Bool), [b]) -> [b]
[] $$ _ = []
(x:xs) $$ (f, ys) = 
    let answer = evaluateDouble f x ys in 
    let zs = xs $$ (f, ys) in 
        answer ++ zs

-- We might need also an operator that is OR: This needs to be changed!

(^^=) :: [a] -> [b] -> [Either a b]
[] ^^= [] = []
(x:xs) ^^= [] = let values = xs ^^= [] in ([Left x] ++  values)
[] ^^= (y:ys) = let values = [] ^^= ys in ([Right y] ++  values)
(x:xs) ^^= (y:ys) = let values = xs ^^= ys in ([Left x, Right y] ++ values)

-- Now we can formulate predicates and execute them in the test sets saved to the global variables. But at the moment output is just a list of elements 
-- that have same type. This means that they form a table. Thus we can execute now only queries that imitate SQL type of queries. Let's consider next the case
-- when the output is a part of the graph. How we can track these situations. At the moment we can query the graph data but the result is just a set without a knowledge of the model.
-- So the solution would be that we get a solution set, we give a model (or we just have information from the solution set because it is a subset of some data that has some model) and then
-- we collect this data so that it follows that model. This should not be difficult: we have a certain way to transform the data into Haskell lists. Let's assume that is is kind of bijective mapping
-- then it has an inverse mapping that we use to map data back to it's original format.

-- The following function populateWithModel takes the query result, as inputs

populateWithModel :: [a] -> (a -> b) -> [(a,b)]
populateWithModel [] _ = []
populateWithModel (x:xs) f = let ys = populateWithModel xs f in (x, f x) : ys

returnAttribute :: (a -> b) -> [a] -> [b]
returnAttribute _ [] = []
returnAttribute f (x:xs) = let ys = returnAttribute f xs in (f x) : ys