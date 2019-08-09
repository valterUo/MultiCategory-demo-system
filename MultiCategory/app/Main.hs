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
import QueryProcessing

encodeListToJSON :: ToJSON a => [a] -> [ByteString]
encodeListToJSON [] = []
encodeListToJSON (x:xs) = (encode x) : (encodeListToJSON xs)

wrapListToJSON :: ToJSON a => [a] -> String
wrapListToJSON xs = "{\"result\":[" ++ L.unpack( L.intercalate (L.pack ", ") (encodeListToJSONText xs)) ++ "]}"

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