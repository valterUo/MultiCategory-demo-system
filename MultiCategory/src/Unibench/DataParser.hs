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
createUnibenchProducts :: [[String]] -> [UnibenchProduct]
createUnibenchProducts [] = []
createUnibenchProducts (x:xs) = (UnibenchProduct (x !! 0)
                                                (x !! 1) 
                                                (readMaybe(x !! 2) :: Maybe Double)  
                                                (x !! 3)
                                                (read(x !! 4) :: Int)
                                                (readMaybe(x !! 5) :: Maybe Int)) : createUnibenchProducts xs

collectUnibenchProducts :: String -> IO([UnibenchProduct])
collectUnibenchProducts filePath = do
    result <- readCSV "|" filePath
    return $ createUnibenchProducts (tail result)

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

-- UnibenchProduct data:

