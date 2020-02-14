module HelsinkiMultiModelRepo.Patent.DataParser where

import HelsinkiMultiModelRepo.Patent.SchemaCategory
import CSVParser
import Data.Monoid
import qualified Data.IntMap.Strict as IntMap
import Text.Read
import Algebra.Graph

---------------------------------------------------------------
-- HelsinkiMultiModelRepo DATA PARSER

-- Functions that work only with HelsinkiMultiModelRepo category.table data

createCategories :: [[String]] -> [(Int, Category)]
createCategories [] = []
createCategories (x:xs) = ((read(x !! 1) :: Int), (Category (read(x !! 0) :: Int) 
                                                            (read(x !! 1) :: Int) 
                                                            (x !! 2) 
                                                            (x !! 3)  
                                                            (x !! 4))): createCategories xs

collectCategories :: FilePath -> IO (IntMap.IntMap Category)
collectCategories path = do
    result <- readCSV ";" path
    return $ IntMap.fromList $ createCategories result

-- Functions that work only with HelsinkiMultiModelRepo assignee.table data

createAssignees :: [[String]] -> [(Int, Assignee)]
createAssignees [] = []
createAssignees (x:xs) = ((read(x !! 0) :: Int), (Assignee (read(x !! 0) :: Int) 
                                                           (x !! 1) 
                                                           (x !! 2) 
                                                           (x !! 3) 
                                                           (readMaybe(x !! 4) :: Maybe Int) 
                                                           (x !! 5) 
                                                           (x !! 6))): createAssignees xs

collectAssignees :: FilePath -> IO(IntMap.IntMap Assignee)
collectAssignees path = do
    result <- readCSV ";" path
    return $ IntMap.fromList $ createAssignees result

-- Functions that work only with HelsinkiMultiModelRepo class.table data: requirment that Category data has been uploaded

createClasses :: [[String]] -> [(Int, Class)]
createClasses [] = []
createClasses (x:xs) = ((read(x !! 0) :: Int), (Class (read(x !! 0) :: Int) 
                                                      (x !! 1) 
                                                      (read(x !! 2) :: Int) 
                                                      (read(x !! 3) :: Int))): createClasses xs

collectClasses :: FilePath -> IO (IntMap.IntMap Class)
collectClasses path = do
    result <- readCSV ";" path
    return $ IntMap.fromList $ createClasses result

-- Functions that work only with Patent (HelsinkiMultiModelRepo data) datatype. The functions uses readMaybe functions to read strings as Int. 
-- If the value is lacking, then the reader substitutes Nothing.

createPatents :: [[String]] -> [(Int, Patent)]
createPatents [] = []
createPatents (patent : patents) = ((read(patent !! 0) :: Int), 
                                    (Patent (read(patent !! 0) :: Int) 
                                        (readMaybe(patent !! 1) :: Maybe Int) 
                                        (readMaybe(patent !! 2) :: Maybe Int) 
                                        (readMaybe(patent !! 3) :: Maybe Int) 
                                        (patent !! 4) 
                                        (patent !! 5) 
                                        (readMaybe(patent !! 6) :: Maybe Int)
                                        (readMaybe(patent !! 7) :: Maybe Int) 
                                        (readMaybe(patent !! 8) :: Maybe Int) 
                                        (readMaybe(patent !! 9) :: Maybe Int)
                                        (readMaybe(patent !! 10) :: Maybe Int)
                                        (readMaybe(patent !! 11) :: Maybe Int)
                                        (readMaybe(patent !! 12) :: Maybe Int) 
                                        (readMaybe(patent !! 13) :: Maybe Int) 
                                        (readMaybe(patent !! 14) :: Maybe Int) 
                                        (readMaybe(patent !! 15) :: Maybe Int) 
                                        (readMaybe(patent !! 16) :: Maybe Int) 
                                        (readMaybe(patent !! 17) :: Maybe Int) 
                                        (readMaybe(patent !! 18) :: Maybe Int) 
                                        (readMaybe(patent !! 19) :: Maybe Int) 
                                        (readMaybe(patent !! 20) :: Maybe Int) 
                                        (readMaybe(patent !! 21) :: Maybe Int) 
                                        (readMaybe(patent !! 22) :: Maybe Int))) : createPatents patents

collectPatents :: FilePath -> IO (IntMap.IntMap Patent)
collectPatents path = do
    result <- readCSV ";" path
    return $ IntMap.fromList $ createPatents result

-- Functions that work only with Inventor (HelsinkiMultiModelRepo data) datatype

createInventors :: [[String]] -> [Inventor]
createInventors [] = []
createInventors (inventor : inventors) = (Inventor 
                                                (read(inventor !! 0) :: Int)
                                                (inventor !! 1) 
                                                (inventor !! 2) 
                                                (inventor !! 3) 
                                                (inventor !! 4) 
                                                (inventor !! 5) 
                                                (inventor !! 6) 
                                                (inventor !! 7) 
                                                (inventor !! 8) 
                                                (inventor !! 9) 
                                                (readMaybe(inventor !! 10) :: Maybe Int)) : createInventors inventors

collectInventors :: FilePath -> IO [Inventor]
collectInventors path = do
    result <- readCSV ";" path
    return $ createInventors result

-- Functions that work only with HelsinkiMultiModelRepo citation.graph data

createIDGraph :: [[String]] -> [(Int, Int)]
createIDGraph [] = []
createIDGraph (x:xs) = (read $ head x :: Int, read $ last x :: Int): createIDGraph xs

populateIDGraph :: [(Int, Int)] -> (Int -> a) -> [(a,a)]
populateIDGraph [] _ = []
populateIDGraph (x:xs) f = let (a, b) = x in (f a, f b) : populateIDGraph xs f

collectPatentGraph :: IntMap.IntMap Patent -> FilePath -> IO(Graph Patent)
collectPatentGraph patents path = do
    result <- readCSV ";" path
    let idGraph = createIDGraph result in
        let populatedGraph = populateIDGraph idGraph (\x -> (patents IntMap.! x)) in
            return $ edges populatedGraph