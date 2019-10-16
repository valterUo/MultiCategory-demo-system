module HelsinkiMultiModelRepo.Patent.DataState where

import qualified Data.IntMap.Strict as IntMap
import Algebra.Graph
import HelsinkiMultiModelRepo.Patent.DataParser
import HelsinkiMultiModelRepo.Patent.SchemaCategory
import System.IO.Unsafe

categories :: IntMap.IntMap Category
categories = unsafePerformIO $ collectCategories "UnibenchData\\category.table"

assignees :: IntMap.IntMap Assignee
assignees = unsafePerformIO $ collectAssignees "UnibenchData\\new_assigneeCUSIP_semicolon.table"

classes :: IntMap.IntMap Class
classes = unsafePerformIO $ collectClasses "UnibenchData\\new_class_semicolon.table"

patents :: IntMap.IntMap Patent
patents = unsafePerformIO $ collectPatents "UnibenchData\\smaller datasets\\patents_new_new_new_semicolon - Copy.table"

patentGraph :: Graph (Maybe Patent)
patentGraph = unsafePerformIO $ collectPatentGraph "UnibenchData\\smaller datasets\\new_citation.graph" patents

inventors :: IntMap.IntMap Inventor
inventors = unsafePerformIO $ collectInventors "UnibenchData\\smaller datasets\\new_inventor_piece.table"