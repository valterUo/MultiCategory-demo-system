module HelsinkiMultiModelRepo.Person.DataState where

import HelsinkiMultiModelRepo.Person.DataParser
import Data.RDF
import System.IO.Unsafe
import RdfFunctions

personGraph :: RDF TList
personGraph = unsafePerformIO $ collectRDF "HelsinkiMultiModelRepoDataSets\\personDataSet\\small_new_personGraph.graph"
