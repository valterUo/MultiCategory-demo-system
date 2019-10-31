module Unibench.DataState where

import Algebra.Graph
import System.IO.Unsafe
import Unibench.DataParser
import qualified Data.IntMap.Strict as IntMap
import Unibench.SchemaCategory

persons :: IntMap.IntMap Person
persons = unsafePerformIO $ collectPersons "UnibenchData\\person_0_0.csv"

posts :: IntMap.IntMap Post
posts = unsafePerformIO $ collectPosts "UnibenchData\\post_1.csv"

unibenchProducts :: [UnibenchProduct]
unibenchProducts = unsafePerformIO $ collectUnibenchProducts "UnibenchData\\new_product.csv"