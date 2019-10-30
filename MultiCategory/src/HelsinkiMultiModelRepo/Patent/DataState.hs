module HelsinkiMultiModelRepo.Patent.DataState where

import qualified Data.IntMap.Strict as IntMap
import Algebra.Graph
import HelsinkiMultiModelRepo.Patent.DataParser
import HelsinkiMultiModelRepo.Patent.SchemaCategory
import System.IO.Unsafe

categories :: IntMap.IntMap Category
categories = unsafePerformIO $ collectCategories "HelsinkiMultiModelRepoDataSets\\patentDataSet\\categories.table"

assignees :: IntMap.IntMap Assignee
assignees = unsafePerformIO $ collectAssignees "HelsinkiMultiModelRepoDataSets\\patentDataSet\\assignees.table"

classes :: IntMap.IntMap Class
classes = unsafePerformIO $ collectClasses "HelsinkiMultiModelRepoDataSets\\patentDataSet\\classes.table"

patents :: IntMap.IntMap Patent
patents = unsafePerformIO $ collectPatents "HelsinkiMultiModelRepoDataSets\\patentDataSet\\patents.table"

patentGraph :: Graph Patent
patentGraph = unsafePerformIO $ collectPatentGraph "HelsinkiMultiModelRepoDataSets\\patentDataSet\\citation.graph" patents

inventors :: [Inventor]
inventors = unsafePerformIO $ collectInventors "HelsinkiMultiModelRepoDataSets\\patentDataSet\\inventors.table"