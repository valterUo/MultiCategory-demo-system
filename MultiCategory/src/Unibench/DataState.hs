module Unibench.DataState where

import Algebra.Graph
import System.IO.Unsafe
import Unibench.DataParser
import qualified Data.IntMap.Strict as IntMap
import Unibench.SchemaCategory
import NimbleGraph.NimbleGraph

persons :: IntMap.IntMap Person
persons = unsafePerformIO $ collectPersons "UnibenchData\\person_0_0.csv"

posts :: IntMap.IntMap Post
posts = unsafePerformIO $ collectPosts "UnibenchData\\post_1.csv"

unibenchProducts :: [UnibenchProduct]
unibenchProducts = unsafePerformIO $ collectUnibenchProducts "UnibenchData\\new_product.csv"

--personCreatedPostGraph :: NimbleGraph (Either Person Post)
--personCreatedPostGraph = unsafePerformIO $ collectPersonPostGraph persons posts "D:\\Unibench-0.2\\Dataset\\SocialNetwork\\post_hasCreator_person_0_0"

--oderlineToUnibenchProduct :: String -> [Invoice] -> Invoice
--oderlineToUnibenchProduct key invoices = head $ foldr (\x xs -> if asin x == key then x:xs else xs) [] products