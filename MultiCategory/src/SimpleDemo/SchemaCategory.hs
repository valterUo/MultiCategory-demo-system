{-# LANGUAGE DeriveGeneric     #-}

module SimpleDemo.SchemaCategory where

import GHC.Generics
import Data.Aeson
import Algebra.Graph
import qualified Data.IntMap.Strict as IntMap

-- OBJECTS are datatypes:

data Customer = Customer { customerId :: Int
    , customerName :: String
    , creditLimit :: Int
    , location :: Int
    } deriving (Show, Eq, Generic)

data Product = Product { productId:: String
    , productName :: String 
    , productPrice :: Int 
    } deriving (Show, Eq, Generic)

data Order = Order { orderNumber :: String
    , orderProducts :: [Product]
    } deriving (Show, Eq, Generic)

data Location = Location { locationId :: Int
    , address :: String
    , cityName :: String
    , zipCode :: Int
    , countryName :: String
    } deriving (Show, Eq, Generic)

-- The following instances allow Aeson package to parse JSON.

instance ToJSON Customer
instance ToJSON Product
instance ToJSON Order
instance ToJSON Location

instance FromJSON Customer
instance FromJSON Product
instance FromJSON Order
instance FromJSON Location

------------------------------------------------------------------------------------------------------------------------
-- MORPHISMS are functions in Haskell:

identity :: a -> a
identity a = a

-- This mapping is derived from the data "34e5e79:1,0cbdf508:2,4dwtfuu:1,3qqqeq9:0,77idy65:3,ery63rg:5". 
-- This is not good. This should be considered as a function obtained from a key-value pairs.

ordered :: Order -> Graph Customer -> Customer
ordered order customers = case orderNumber order of 
        "34e5e79" -> head $ foldg [] (\customer -> if customerId customer ==  1 then [customer] else []) (++) (++) customers
        "0cbdf508" -> head $ foldg [] (\customer -> if customerId customer ==  2 then [customer] else []) (++) (++) customers
        "4dwtfuu" -> head $ foldg [] (\customer -> if customerId customer ==  1 then [customer] else []) (++) (++) customers
        "3qqqeq9" -> head $ foldg [] (\customer -> if customerId customer ==  0 then [customer] else []) (++) (++) customers
        "77idy65" -> head $ foldg [] (\customer -> if customerId customer ==  3 then [customer] else []) (++) (++) customers
        "ery63rg" -> head $ foldg [] (\customer -> if customerId customer ==  5 then [customer] else []) (++) (++) customers
        "4839fh" -> head $ foldg [] (\customer -> if customerId customer ==  6 then [customer] else []) (++) (++) customers
        "reuihf54" -> head $ foldg [] (\customer -> if customerId customer ==  7 then [customer] else []) (++) (++) customers

-- For the following morphisms the first input is assumed to be fixed.
-- For each customer and for each order, there is a following kind of morphisms:

knows :: Customer -> Customer -> Graph Customer -> Bool
knows customer1 customer2 customers = hasEdge customer1 customer2 customers

located :: Customer -> (IntMap.IntMap Location) -> Location
located customer locations = locations IntMap.! (location customer)

-- Now it follows that edgeInGraph2 customer1 customer2 customerId customerGraph == knows customer1 customer2

contains :: Order -> Product -> Bool
contains a b = elem b (orderProducts a)

-- Generally we can produce arbitrarily many morphims using the following function for each possible predicate in a query.

predicateMorphism :: a -> (a -> Bool) -> Bool
predicateMorphism a f = f a