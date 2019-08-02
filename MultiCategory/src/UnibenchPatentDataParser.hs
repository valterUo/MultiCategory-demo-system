module UnibenchPatentDataParser where

import UnibenchPatentSchemaCategory
import SQLParser
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Text.Read
import Algebra.Graph

---------------------------------------------------------------
-- UNIBENCH DATA PARSER

-- Functions that work only with Unibench category.table data

createCategories :: [[String]] -> [Category]
createCategories [] = []
createCategories (x:xs) = (Category (read(x !! 0) :: Int) (read(x !! 1) :: Int) (x !! 2) (x !! 3)  (x !! 4)): createCategories xs

collectCategories :: FilePath -> IO [Category]
collectCategories path = do
    result <- readCSV path
    return $ createCategories result

-- Functions that work only with Unibench assignee.table data

createAssignees :: [[String]] -> [Assignee]
createAssignees [] = []
createAssignees (x:xs) = (Assignee (read(x !! 0) :: Int) (x !! 1) (x !! 2) (x !! 3) (readMaybe(x !! 4) :: Maybe Int) (x !! 5) (x !! 6)): createAssignees xs

collectAssignees :: FilePath -> IO [Assignee]
collectAssignees path = do
    result <- readCSV path
    return $ createAssignees result

-- Functions that work only with Unibench class.table data: requirment that Category data has been uploaded

findCategoryById :: [Category] -> Int -> Maybe Category
findCategoryById [] _ = Nothing
findCategoryById (x:xs) id = if catId x == id then Just x else findCategoryById xs id

createClasses :: [[String]] -> [Category] -> [Class]
createClasses [] _ = []
createClasses (x:xs) categories = (Class (read(x !! 0) :: Int) (x !! 1) (findCategoryById categories (read(x !! 2) :: Int)) (findCategoryById categories (read(x !! 3) :: Int))): createClasses xs categories

collectClasses :: FilePath -> [Category] -> IO [Class]
collectClasses path categories = do
    result <- readCSV path
    return $ createClasses result categories

-- Functions that work only with Patent (Unibench data) datatype. The functions uses readMaybe functions to read strings as Int. If the value is lacking, then the reader substitutes Nothing.

findAssigneeById :: [Assignee] -> Maybe Int -> Maybe Assignee
findAssigneeById [] _ = Nothing
findAssigneeById _ Nothing = Nothing
findAssigneeById (x:xs) (Just id) = if assigneeId x == id then Just x else findAssigneeById xs (Just id)

findClassById :: [Class] -> Maybe Int -> Maybe Class
findClassById [] _ = Nothing
findClassById _ Nothing = Nothing
findClassById (x:xs) (Just id) = if classId x == id then Just x else findClassById xs (Just id)

safeFindCategoryById :: [Category] -> Maybe Int -> Maybe Category
safeFindCategoryById [] _ = Nothing
safeFindCategoryById _ Nothing = Nothing
safeFindCategoryById (x:xs) (Just id) = if catId x == id then Just x else safeFindCategoryById xs (Just id)

createPatents :: [[String]] -> [Assignee] -> [Class] -> [Category] -> [(Int, Patent)]
createPatents [] _ _ _ = []
createPatents (patent : patents) assignees classes categories =((read(patent !! 0) :: Int), 
                                    (Patent 
                                        (read(patent !! 0) :: Int) 
                                        (readMaybe(patent !! 1) :: Maybe Int) 
                                        (readMaybe(patent !! 2) :: Maybe Int) 
                                        (readMaybe(patent !! 3) :: Maybe Int) 
                                        (patent !! 4) 
                                        (patent !! 5) 
                                        (findAssigneeById assignees (readMaybe(patent !! 6) :: Maybe Int)) 
                                        (readMaybe(patent !! 7) :: Maybe Int) 
                                        (readMaybe(patent !! 8) :: Maybe Int) 
                                        (findClassById classes (readMaybe(patent !! 9) :: Maybe Int))
                                        (safeFindCategoryById categories (readMaybe(patent !! 10) :: Maybe Int))
                                        (safeFindCategoryById categories (readMaybe(patent !! 11) :: Maybe Int))
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
                                        (readMaybe(patent !! 22) :: Maybe Int))) : createPatents patents assignees classes categories

collectPatents :: FilePath -> [Assignee] -> [Class] -> [Category] -> IO (IntMap.IntMap Patent)
collectPatents path assignees classes categories = do
    result <- readCSV path
    return $ IntMap.fromList $ createPatents result assignees classes categories

-- Functions that work only with Inventor (Unibench data) datatype

createInventors :: [[String]] -> IntMap.IntMap Patent -> [Inventor]
createInventors [] _ = []
createInventors (inventor : inventors) map = (Inventor 
                                                (IntMap.lookup (read(inventor !! 0) :: Int) map)
                                                (inventor !! 1) 
                                                (inventor !! 2) 
                                                (inventor !! 3) 
                                                (inventor !! 4) 
                                                (inventor !! 5) 
                                                (inventor !! 6) 
                                                (inventor !! 7) 
                                                (inventor !! 8) 
                                                (inventor !! 9) 
                                                (readMaybe(inventor !! 10) :: Maybe Int)) : createInventors inventors map

collectInventors :: FilePath -> IntMap.IntMap Patent -> IO [Inventor]
collectInventors path map = do
    result <- readCSV path
    return $ createInventors result map

-- Functions that work only with Unibench citation.graph data

createIDGraph :: [[String]] -> [(Int, Int)]
createIDGraph [] = []
createIDGraph (x:xs) = (read $ head x :: Int, read $ last x :: Int): createIDGraph xs

populateIDGraph :: [(Int, Int)] -> (Int -> a) -> [(a,a)]
populateIDGraph [] _ = []
populateIDGraph (x:xs) f = let (a, b) = x in (f a, f b) : populateIDGraph xs f

collectPatentGraph :: FilePath -> IntMap.IntMap Patent -> IO(Graph (Maybe Patent))
collectPatentGraph path patents = do
    result <- readCSV path
    let idGraph = createIDGraph result in
        let populatedGraph = populateIDGraph idGraph (\x -> IntMap.lookup x patents) in
            return $ edges populatedGraph