module Unibench.Patent.DataState where

import qualified Data.IntMap.Strict as IntMap
import Algebra.Graph
import Unibench.Patent.DataParser
import Unibench.Patent.SchemaCategory
import System.IO.Unsafe

categories :: [Category]
categories = unsafePerformIO $ collectCategories "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\category.table"

assignees :: [Assignee]
assignees = unsafePerformIO $ collectAssignees "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\new_assigneeCUSIP_semicolon.table"

classes :: [Class]
classes = unsafePerformIO $ collectClasses "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\new_class_semicolon.table" categories

patents :: IntMap.IntMap Patent
patents = unsafePerformIO $ collectPatents "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\smaller datasets\\patents_new_new_new_semicolon - Copy.table" assignees classes categories

patentGraph :: Graph (Maybe Patent)
patentGraph = unsafePerformIO $ collectPatentGraph "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\smaller datasets\\new_citation.graph" patents

inventors :: [Inventor]
inventors = unsafePerformIO $ collectInventors "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\smaller datasets\\new_inventor_piece.table" patents

collectAllPatents = do
    print $ patents IntMap.!? 3405204
    -- categories <- collectCategories "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\category.table"
    -- print $ length categories 
    -- assignees <- collectAssignees "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\new_assigneeCUSIP_semicolon.table"
    -- print $ length assignees
    -- classes <- collectClasses "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\new_class_semicolon.table" categories
    -- print $ length classes
    -- patents <- collectPatents "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\smaller datasets\\new_patent_semicolon.table" assignees classes categories
    -- print $ IntMap.size patents
    -- patentGraph <- collectPatentGraph "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\smaller datasets\\new_citation.graph" patents
    -- print $ hasEdge (IntMap.lookup 3858243 patents) (IntMap.lookup 2949611 patents) patentGraph
    -- inventors <- collectInventors "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory\\Unibench\\smaller datasets\\new_inventor_piece.table" patents
    -- print $ length inventors