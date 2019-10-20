module HelsinkiMultiModelRepo.Patent.DataState where

import qualified Data.IntMap.Strict as IntMap
import Algebra.Graph
import HelsinkiMultiModelRepo.Patent.DataParser
import HelsinkiMultiModelRepo.Patent.SchemaCategory
import System.IO.Unsafe

categories :: IntMap.IntMap Category
categories = unsafePerformIO $ collectCategories "HelsinkiMultiModelRepoDataSets\\patentDataSet\\category.table"

assignees :: IntMap.IntMap Assignee
assignees = unsafePerformIO $ collectAssignees "HelsinkiMultiModelRepoDataSets\\patentDataSet\\new_assigneeCUSIP_semicolon.table"

classes :: IntMap.IntMap Class
classes = unsafePerformIO $ collectClasses "HelsinkiMultiModelRepoDataSets\\patentDataSet\\new_class_semicolon.table"

patents :: IntMap.IntMap Patent
patents = unsafePerformIO $ collectPatents "HelsinkiMultiModelRepoDataSets\\patentDataSet\\patents_new_new_new_semicolon - Copy.table"

patentGraph :: Graph (Maybe Patent)
patentGraph = unsafePerformIO $ collectPatentGraph "HelsinkiMultiModelRepoDataSets\\patentDataSet\\new_citation.graph" patents

inventors :: IntMap.IntMap Inventor
inventors = unsafePerformIO $ collectInventors "HelsinkiMultiModelRepoDataSets\\patentDataSet\\new_inventor_piece.table"