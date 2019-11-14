module HelsinkiMultiModelRepo.University.DataState where

import System.IO.Unsafe
import RdfFunctions
import qualified Data.IntMap.Strict as IntMap
import HelsinkiMultiModelRepo.University.SchemaCategory
import HelsinkiMultiModelRepo.University.DataParser
import Data.RDF

universities :: IntMap.IntMap University
universities = unsafePerformIO $ collectUniversities "D:\\Helsinki Multi-Model Datasets\\University_dataset\\University_dataset\\processed_dataset\\university_scorecard.table"

famousLocations :: RDF TList
famousLocations = unsafePerformIO $ collectRDF "D:\\Helsinki Multi-Model Datasets\\University_dataset\\University_dataset\\processed_dataset\\new_locations.graph"