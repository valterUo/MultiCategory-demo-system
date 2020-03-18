module SimpleDemo.InstanceCategory where

import SimpleDemo.SchemaCategory
import SimpleDemo.DataParser
import Algebra.Graph
import System.IO.Unsafe
import qualified Data.IntMap.Strict as IntMap

customerVertexFile = "demoData//customerVertex.csv"
customerEdgeFile = "demoData//customerEdge.csv"
locationsFile = "demoData//locationsTable.csv"
ordersFile = "demoData//orders.xml"

-- Data is in the global variables.

customers :: Graph Customer
customers = unsafePerformIO $ collectCustomers customerVertexFile customerEdgeFile

locations :: IntMap.IntMap Location
locations = unsafePerformIO $ collectLocations locationsFile

orders :: [Order]
orders = unsafePerformIO $ readXMLFile ordersFile