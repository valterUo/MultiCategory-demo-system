module Main where

import Data.List
import Data.ByteString.Lazy.Internal
import System.IO
import Control.Monad
import XMLParser
import SchemaCategory
import SQLParser
import Data.Aeson
import D3jsGraphParser
import qualified Data.Text.Lazy as L
import Algebra.Graph
import UnibenchPatentSchemaCategory
import qualified Data.IntMap.Strict as IntMap
import UnibenchPatentDataParser

------------------------------------------------------------------------------------------------------------------------

-- Instead of this we would like to input data in right format and then parse it to following way String -> Haskell list -> String. Assume that we input tables as csv files. Each file contain a table.
-- Each row fits to some data type, in this case Customer. We assume that attributes have an ordering that is same as ordering in the definition of the corresponding datatype.

------------------------------------------------------------------------------------------------------------------------
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

encodeListToJSON :: ToJSON a => [a] -> [ByteString]
encodeListToJSON [] = []
encodeListToJSON (x:xs) = (encode x) : (encodeListToJSON xs)

wrapListToJSON :: ToJSON a => [a] -> String
wrapListToJSON xs = "{\"result\":[" ++ L.unpack( L.intercalate (L.pack ", ") (encodeListToJSONText xs)) ++ "]}"

--decide :: a -> Mult b
--decide x = undefined

-- This function is reponsible of taking in data and a graph defined as a function a-> b where for each data point it assign a node in a graph namely b.
-- embedToGraph :: Maybe [a] -> (a -> b) -> Maybe [(a, b)]

-- Another challenge is that what if we have solution set that contains multiple models. For example: find all the friends of "Mary" and their order id's. Order id's are inside XML but customers
-- are in graph. How do we present this data?

------------------------------------------------------------------------------------------------------------------------

-- doubleFoldrList :: (a -> a -> b) -> a -> [a] -> (b -> b -> c) -> 

main = do
    -- print $ foldr (\order maxPrices -> ((foldr (\product maxProduct -> if productPrice product > maxProduct then productPrice product else maxProduct) 0 (orderProducts order)), orderNumber order):maxPrices) [] orders
    -- print $ foldr (\x xs -> ((orderNumber x, customerName $ ordered x customers):xs)) [] orders
    -- let alices = foldr (\x xs -> if customerName x == "Alice" then x:xs else xs) [] customers in
    --    print $ alices ++ foldr (\x xs -> (foldr (\y ys -> if knows x y then x:ys else ys) [] alices) ++ xs) [] customers
    --outputStrings <- readCSV "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\inventor.table"
    -- inventors <- collectInventors "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_inventor.table"
    -- print (length inventors)
    -- let graph = edges [(1,6), (3,6), (6,3), (3,1), (1,2), (0,5), (4,2), (4,5)] in
    --    print $ isSubgraphOf (path [6, 3, 1, 2, 4]) graph
    --graph <- parseCSVGraph "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_citation.graph" (\x -> head(findInventor x inventors))
    --print $ size graph
    categories <- collectCategories "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\processed_dataset\\category.table"
    print $ length categories 
    assignees <- collectAssignees "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_assignee_semicolon.table"
    print $ length assignees
    classes <- collectClasses "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_class_semicolon.table" categories
    print $ length classes
    patents <- collectPatents "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_patent_semicolon.table" assignees classes categories
    print $ IntMap.size patents
    patentGraph <- collectPatentGraph "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_citation.graph" patents
    print $ hasEdge (IntMap.lookup 3858243 patents) (IntMap.lookup 2949611 patents) patentGraph
    inventors <- collectInventors "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_inventor_piece.table" patents
    print $ length inventors