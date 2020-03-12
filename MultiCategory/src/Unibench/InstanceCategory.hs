module Unibench.InstanceCategory where

import Algebra.Graph
import System.IO.Unsafe
import Unibench.DataParser
import qualified Data.IntMap.Strict as IntMap
import Unibench.SchemaCategory
import NimbleGraph.NimbleGraph
import qualified Data.HashMap.Strict as HashMap

persons :: IntMap.IntMap Person
persons = unsafePerformIO $ collectPersons "UnibenchData\\person_table.csv"

posts :: IntMap.IntMap Post
posts = unsafePerformIO $ collectPosts "UnibenchData\\post_1.csv"

unibenchProducts :: HashMap.HashMap String UnibenchProduct
unibenchProducts = unsafePerformIO $ collectUnibenchProducts "UnibenchData\\small_product.csv"

unibenchOrders :: [UnibenchOrder]
unibenchOrders = unsafePerformIO $ collectUnibenchOrders "UnibenchData\\small_order.json"

personKnowsPersonGraph :: NimbleGraph Person CreationDate
personKnowsPersonGraph = unsafePerformIO $ collectPersonKnowsPersonGraph persons "UnibenchData\\person_knows_person_graph.csv"

personCreatedPostGraph :: NimbleGraph (Either Post Person) (Maybe String)
personCreatedPostGraph = unsafePerformIO $ collectPersonPostGraph persons posts "UnibenchData\\post_hasCreator_person.csv"

postHasProductGraph :: NimbleGraph (Either Post UnibenchProduct) (Maybe String)
postHasProductGraph = unsafePerformIO $ collectPostProductGraph posts unibenchProducts "UnibenchData\\post_to_product.csv"

personToProductGraph :: NimbleGraph (Either Person UnibenchProduct) (Maybe String)
personToProductGraph = unsafePerformIO $ collectPersonToProductGraph persons unibenchProducts "UnibenchData\\person_to_product.csv"

feedbacks :: [Feedback]
feedbacks = unsafePerformIO $ collectFeedbacks "UnibenchData\\small_feedback.csv"

vendors :: [Vendor]
vendors = unsafePerformIO $ collectVendors "UnibenchData\\Vendor.csv"

invoices :: [Invoice]
invoices = unsafePerformIO $ collectInvoices "UnibenchData\\small_invoice.xml"