{-# LANGUAGE OverloadedStrings #-}

module Unibench.DataParser where

import Algebra.Graph
import CSVParser
import Unibench.SchemaCategory
import qualified Data.IntMap.Strict as IntMap
import Text.Read
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Internal as B
import Data.Aeson
import NimbleGraph.NimbleGraph
import qualified Data.HashMap.Strict as HashMap
import Xeno.DOM
import XMLParser
import qualified Data.ByteString.Char8 as C8
import Debug.Trace

-- Person data:
createPersons :: [[String]] -> [(Int, Person)]
createPersons [] = []
createPersons (x:xs) = ((read(x !! 0) :: Int), Person (read(x !! 0) :: Int) 
                                                      (x !! 1) 
                                                      (x !! 2)
                                                      (x !! 3)
                                                      (x !! 4)
                                                      (x !! 5)
                                                      (x !! 6) 
                                                      (x !! 7)
                                                      (read(x !! 8) :: Int)) : createPersons xs

collectPersons :: String -> IO(IntMap.IntMap Person)
collectPersons filePath = do
    result <- readCSV "|" filePath
    return $ IntMap.fromList $ createPersons (tail result)

-- Post data:
createPosts :: [[String]] -> [(Int, Post)]
createPosts [] = []
createPosts (x:xs) = ((read(x !! 0) :: Int), Post (read(x !! 0) :: Int) 
                                                  (x !! 2) 
                                                  (x !! 3) 
                                                  (x !! 4)
                                                  (x !! 5)
                                                  (x !! 6)
                                                  (readMaybe(x !! 7) :: Maybe Int)) : createPosts xs

collectPosts :: String -> IO(IntMap.IntMap Post)
collectPosts filePath = do
    result <- readCSV "|" filePath
    return $ IntMap.fromList $ createPosts (tail result)

-- UnibenchProduct data:
createUnibenchProducts :: [[String]] -> [(String, UnibenchProduct)]
createUnibenchProducts [] = []
createUnibenchProducts (x:xs) = (x !! 0, UnibenchProduct (x !! 0)
                                                (x !! 1) 
                                                (readMaybe(x !! 2) :: Maybe Double)  
                                                (x !! 3)
                                                (read(x !! 4) :: Int)
                                                (readMaybe(x !! 5) :: Maybe Int)) : createUnibenchProducts xs

collectUnibenchProducts :: String -> IO(HashMap.HashMap String UnibenchProduct)
collectUnibenchProducts filePath = do
    result <- readCSV "|" filePath
    return $ HashMap.fromList $ createUnibenchProducts (tail result)

-- UnibenchOrder data:
createUnibenchOrder :: [C.ByteString] -> IO([UnibenchOrder])
createUnibenchOrder [] = return []
createUnibenchOrder (x:xs) = case (decode(B.packChars(C.unpack x)) :: Maybe UnibenchOrder) of
    Nothing -> createUnibenchOrder xs
    Just(order) -> do
        print order
        orders <- createUnibenchOrder xs 
        return $ (order):(orders)

collectUnibenchOrders :: FilePath -> IO([UnibenchOrder])
collectUnibenchOrders path = do 
    content <- C.readFile path
    let linesOfFile = C.lines content in do
        print $ linesOfFile !! 0
        result <- createUnibenchOrder linesOfFile
        return result

-- Unibench Person - knows -> Person graph data: (id, source, target, labels, value)

data CreationDate = CreationDate String deriving Show

createPersonKnowsPersonGraph :: (IntMap.IntMap Person) -> [[String]] -> [(String, (String, Person), (String, Person), [String], CreationDate)]
createPersonKnowsPersonGraph _ [] = []
createPersonKnowsPersonGraph persons (link:links) = let personSource = (read(link !! 0) :: Int) in
    let personTarget = (read(link !! 1) :: Int) in
        case persons IntMap.!? personSource of
            Nothing -> createPersonKnowsPersonGraph persons links
            Just (source) -> case persons IntMap.!? personTarget of 
                Nothing -> createPersonKnowsPersonGraph persons links
                Just(target) -> ((link !! 0)++(link !! 1), (link !! 0, source), (link !! 1, target), ["knows"], CreationDate (link !! 2)) : (createPersonKnowsPersonGraph persons links)

collectPersonKnowsPersonGraph :: (IntMap.IntMap Person) -> FilePath -> IO(NimbleGraph Person CreationDate)
collectPersonKnowsPersonGraph persons filePath = do
    result <- readCSV "|" filePath
    return $ mkGraphFromTuples $ createPersonKnowsPersonGraph persons (tail result)

-- Unibench Post - hasCreator -> Person graph data:
createPersonPostGraph :: (IntMap.IntMap Person) -> (IntMap.IntMap Post) -> [[String]] -> [(String, (String, (Either Post Person)), (String, (Either Post Person)), [String], Maybe String)]
createPersonPostGraph _ _ [] = []
createPersonPostGraph persons posts (link:links) = let personKey = (read(link !! 1) :: Int) in
    let postKey = (read(link !! 0) :: Int) in
        case posts IntMap.!? postKey of
            Nothing -> createPersonPostGraph persons posts links
            Just (post) -> case persons IntMap.!? personKey of 
                Nothing -> createPersonPostGraph persons posts links
                Just(person) -> ((link !! 0)++(link !! 1), (link !! 0, Left post), (link !! 1, Right person), ["has_been_created_by"], Nothing) : (createPersonPostGraph persons posts links)

collectPersonPostGraph :: (IntMap.IntMap Person) -> (IntMap.IntMap Post) -> FilePath -> IO(NimbleGraph (Either Post Person) (Maybe String))
collectPersonPostGraph persons posts filePath = do
    result <- readCSV "|" filePath
    return $ mkGraphFromTuples $ createPersonPostGraph persons posts (tail result)

-- Unibench Post - has -> Product
createPostProductGraph :: (IntMap.IntMap Post) -> (HashMap.HashMap String UnibenchProduct) -> [[String]] -> [(String, (String, (Either Post UnibenchProduct)), (String, (Either Post UnibenchProduct)), [String], Maybe String)]
createPostProductGraph _ _ [] = []
createPostProductGraph posts products (link:links) = let postKey = (read(link !! 0) :: Int) in
    let productKey = (link !! 1) in
        case posts IntMap.!? postKey of
            Nothing -> createPostProductGraph posts products links
            Just (post) -> case HashMap.lookup productKey products of 
                Nothing -> createPostProductGraph posts products links
                Just(product) -> ((link !! 0)++(link !! 1), (link !! 0, Left post), (link !! 1, Right product), ["has_product"], Nothing) : (createPostProductGraph posts products links)

collectPostProductGraph :: (IntMap.IntMap Post) -> (HashMap.HashMap String UnibenchProduct) -> FilePath -> IO(NimbleGraph (Either Post UnibenchProduct) (Maybe String))
collectPostProductGraph posts products filePath = do
    result <- readCSV "|" filePath
    return $ mkGraphFromTuples $ createPostProductGraph posts products (tail result)

-- Unibench Person - has_interest -> Product
createPersonToProductGraph :: (IntMap.IntMap Person) -> (HashMap.HashMap String UnibenchProduct) -> [[String]] -> [(String, (String, (Either Person UnibenchProduct)), (String, (Either Person UnibenchProduct)), [String], Maybe String)]
createPersonToProductGraph _ _ [] = []
createPersonToProductGraph persons products (link:links) = let personKey = (read(link !! 0) :: Int) in
    let productKey = (link !! 1) in
        case persons IntMap.!? personKey of
            Nothing -> createPersonToProductGraph persons products links
            Just (person) -> case HashMap.lookup productKey products of 
                Nothing -> createPersonToProductGraph persons products links
                Just(product) -> ((link !! 0)++(link !! 1), (link !! 0, Left person), (link !! 1, Right product), ["has_interest"], Nothing) : (createPersonToProductGraph persons products links)

collectPersonToProductGraph :: (IntMap.IntMap Person) -> (HashMap.HashMap String UnibenchProduct) -> FilePath -> IO(NimbleGraph (Either Person UnibenchProduct) (Maybe String))
collectPersonToProductGraph persons products filePath = do
    result <- readCSV "|" filePath
    return $ mkGraphFromTuples $ createPersonToProductGraph persons products (tail result)

-- Unibench Feedback data:

createFeedback :: [[String]] -> [Feedback]
createFeedback [] = []
createFeedback (x:xs) = (Feedback (x !! 0) (read(x !! 1) :: Int) (x !! 2)) : createFeedback xs

collectFeedbacks :: String -> IO([Feedback])
collectFeedbacks filePath = do
    result <- readCSV "|" filePath
    return $ createFeedback result

-- Unibench Vendor data:

createVendor :: [[String]] -> [Vendor]
createVendor [] = []
createVendor (x:xs) = (Vendor (x !! 0) (x !! 1) (x !! 2)) : createVendor xs

collectVendors :: String -> IO([Vendor])
collectVendors filePath = do
    result <- readCSV "," filePath
    return $ createVendor $ tail result

-- Unibench Invoice data:

collectProduct :: Node -> Maybe UnibenchOrderline
collectProduct node = if allChildrenLeaves (children node)
    then let x = collectDataFromLeaves(children(node)) in Just (UnibenchOrderline (x !! 0)
                                                                        (x !! 1) 
                                                                        (x !! 2)  
                                                                        (read(x !! 3) :: Double)
                                                                        (x !! 4))
    else Nothing

collectProducts :: [Node] -> Maybe [UnibenchOrderline]
collectProducts [] = Just []
collectProducts (n:nodes) = case collectProduct n of
    Nothing -> collectProducts nodes
    Just x -> let Just tailProducts = collectProducts nodes in 
                    Just (x : tailProducts)

collectOrder :: Node -> UnibenchOrder
collectOrder node = let rootChildren = children node in 
    let Just products = collectProducts (tail (children node)) in
        let Just content = unwrapMultipleNodes rootChildren in
            UnibenchOrder (content !! 0) (content !! 1) (content !! 2) (read(content !! 3) :: Double) products

collectOrders :: [Node] -> [Invoice]
collectOrders [] = []
collectOrders (order:orders) = (Invoice $ collectOrder order) : (collectOrders orders)

collectInvoices :: FilePath -> IO [Invoice]
collectInvoices path = do
    xmlData <- readFile path
    let Right node = parse(C8.pack xmlData) in 
        return $ collectOrders $ children node