module Unibench.DataState where

import Algebra.Graph
import System.IO.Unsafe
import Unibench.DataParser
import qualified Data.IntMap.Strict as IntMap
import Unibench.SchemaCategory
import NimbleGraph.NimbleGraph
import qualified Data.HashMap.Strict as HashMap

persons :: IntMap.IntMap Person
persons = unsafePerformIO $ collectPersons "UnibenchData\\person.csv"

posts :: IntMap.IntMap Post
posts = unsafePerformIO $ collectPosts "UnibenchData\\post_1.csv"

unibenchProducts :: HashMap.HashMap String UnibenchProduct
unibenchProducts = unsafePerformIO $ collectUnibenchProducts "UnibenchData\\new_product.csv"

personCreatedPostGraph :: NimbleGraph (Either Post Person) (Maybe String)
personCreatedPostGraph = unsafePerformIO $ collectPersonPostGraph persons posts "UnibenchData\\post_hasCreator_person.csv"

postHasProductGraph :: NimbleGraph (Either Post UnibenchProduct) (Maybe String)
postHasProductGraph = unsafePerformIO $ collectPostProductGraph posts unibenchProducts "UnibenchData\\post_to_product.csv"

personToProductGraph :: NimbleGraph (Either Person UnibenchProduct) (Maybe String)
personToProductGraph = unsafePerformIO $ collectPersonToProductGraph persons unibenchProducts "UnibenchData\\person_to_product.csv"

--oderlineToUnibenchProduct :: String -> [Invoice] -> Invoice
--oderlineToUnibenchProduct key invoices = head $ foldr (\x xs -> if asin x == key then x:xs else xs) [] products