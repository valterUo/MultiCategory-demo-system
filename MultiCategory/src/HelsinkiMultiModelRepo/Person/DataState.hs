module HelsinkiMultiModelRepo.Person.DataState where

import HelsinkiMultiModelRepo.Person.DataParser
import Data.RDF
import System.IO.Unsafe
import RdfFunctions

personGraph :: RDF TList
personGraph = unsafePerformIO $ collectRDF "D:\\Helsinki Multi-Model Datasets\\Person_dataset\\Person_dataset\\reprocessed_dataset\\small_new_personGraph.graph"
