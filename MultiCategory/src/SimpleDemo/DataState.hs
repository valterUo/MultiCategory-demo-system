{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SimpleDemo.DataState where

import SimpleDemo.SchemaCategory
import Algebra.Graph
import CSVParser
import XMLParser
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as C8
import Xeno.DOM
import qualified Data.IntMap.Strict as IntMap

customerVertexFile = "demoData\\customerVertex.csv"
customerEdgeFile = "demoData\\customerEdge.csv"
locationsFile = "demoData\\locationsTable.csv"
ordersFile = "demoData\\orders.xml"

-- Data is in the global variables.

customers :: Graph Customer
customers = unsafePerformIO $ collectCustomers customerVertexFile customerEdgeFile

locations :: IntMap.IntMap Location
locations = unsafePerformIO $ collectLocations locationsFile

orders :: [Order]
orders = unsafePerformIO $ readXMLFile ordersFile

-- Graph data:
createCustomerVertices :: [[String]] -> [Customer]
createCustomerVertices [] = []
createCustomerVertices (x:xs) = (Customer (read (x !! 0) :: Int) (x !! 1) (read (x !! 2) :: Int) (read (x !! 3) :: Int)) : createCustomerVertices xs

findCustomerID :: Int -> [Customer] -> Customer
findCustomerID i customers = head $ filter (\x -> if customerId x == i then True else False) customers

createCustomerEdges :: [[String]] -> [Customer] -> [(Customer, Customer)]
createCustomerEdges [] _ = []
createCustomerEdges (x:xs) vertices = let (i, j) = (read(x !! 0) :: Int, read(x !! 1) :: Int) 
    in (findCustomerID i vertices, findCustomerID j vertices) : (createCustomerEdges xs vertices)

collectCustomers :: String -> String -> IO(Graph Customer)
collectCustomers vertexFilePath edgeFilePath = do
    customerVertices <- readCSV ";" vertexFilePath
    customerEdges <- readCSV ";" edgeFilePath
    return $ edges(createCustomerEdges (tail customerEdges) (createCustomerVertices $ tail customerVertices))

-- Relational data:

createLocations :: [[String]] -> [(Int, Location)]
createLocations [] = []
createLocations (x:xs) = ((read(x !! 0) :: Int), Location (read(x !! 0) :: Int) (x !! 1) (x !! 2) (read(x !! 3) :: Int) (x !! 4)) : createLocations xs

collectLocations :: String -> IO(IntMap.IntMap Location)
collectLocations locationFilePath = do
    locationList <- readCSV ";" locationFilePath
    return $ IntMap.fromList $ createLocations (tail locationList)

-- XML data:
collectProduct :: Node -> Maybe Product
collectProduct node = if allChildrenLeaves (children node)
    then let content = collectDataFromLeaves(children(node)) in Just (Product (content !! 0) (content !! 1) (read (content !! 2) :: Int))
    else Nothing

collectProducts :: [Node] -> Maybe [Product]
collectProducts [] = Just []
collectProducts (n:nodes) = case collectProduct n of
    Nothing -> Nothing
    Just x -> let Just tailProducts = collectProducts nodes in Just (x : tailProducts)

collectOrder :: Node -> Order
collectOrder node = let firstChild = head (children node) in
    let Just products = collectProducts (tail (children node)) in
    let Just orderNum = unwrapLeafContent(contents firstChild) in
    Order (orderNum) (products)

collectOrders :: [Node] -> [Order]
collectOrders [] = []
collectOrders (order:orders) = (collectOrder order) : (collectOrders orders)

xmlTreeToList :: Node -> [Order]
xmlTreeToList node = collectOrders(children node)

readXMLFile :: FilePath -> IO [Order]
readXMLFile path = do
    xmlData <- readFile path
    let Right node = parse(C8.pack xmlData) in return $ xmlTreeToList node

returnProducts :: [Order] -> [Product]
returnProducts [] = []
returnProducts (order:orders) = addListToListAsSet (orderProducts order) (returnProducts orders)