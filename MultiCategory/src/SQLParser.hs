module SQLParser where

import SchemaCategory
import UnibenchPatentSchemaCategory
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Text.Read
import Algebra.Graph

-- General CSV file parser

readCSV :: FilePath -> IO [[String]]
readCSV path = do str <- readFile path
                  return $ map process $ lines str
  where process xs = case break (==';') xs of (a,[])    -> [a]
                                              (a,';':b) -> a:process b

---------------------------------------------------------------
-- SIMPLE DEMO DATA
-- Functions that work only with Customer datatype                                              

-- createCustomers:: [[String]] -> [Customer]
-- createCustomers [] = []
-- createCustomers (customer:customers) = (Customer (read(customer !! 0) :: Int) (customer !! 1) (read(customer !! 2) :: Int)) : createCustomers customers

-- collectCustomers:: FilePath -> IO [Customer]
-- collectCustomers path = do
--     result <- readCSV path
--     return $ createCustomers result