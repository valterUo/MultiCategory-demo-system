module HelsinkiMultiModelRepo.Person.DataState where

import HelsinkiMultiModelRepo.Person.DataParser
import qualified Data.RDF as RDF
import System.IO.Unsafe

personGraph :: RDF.RDF RDF.TList
personGraph = unsafePerformIO $ collectPersonGraph "D:\\Helsinki Multi-Model Datasets\\Person_dataset\\Person_dataset\\reprocessed_dataset\\small_new_personGraph.graph"
