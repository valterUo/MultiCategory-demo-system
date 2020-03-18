module HelsinkiMultiModelRepo.University.InstanceCategory where

import System.IO.Unsafe
import RdfFunctions
import qualified Data.IntMap.Strict as IntMap
import HelsinkiMultiModelRepo.University.SchemaCategory
import HelsinkiMultiModelRepo.University.DataParser
import Data.RDF

universities :: IntMap.IntMap University
universities = unsafePerformIO $ collectUniversities "HelsinkiMultiModelRepoDataSets//universityDataSet//university_scorecard.table"

famousLocations :: RDF TList
famousLocations = unsafePerformIO $ collectRDF "HelsinkiMultiModelRepoDataSets//universityDataSet//new_locations.graph"