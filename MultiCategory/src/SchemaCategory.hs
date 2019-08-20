{-# LANGUAGE DeriveGeneric     #-}

module SchemaCategory where

import GHC.Generics
import Data.Aeson
import Algebra.Graph

-- Old style: Data is just in global variables.
-- Graph
customers = edges [(Customer 1 "John" 2000 10, Customer 6 "Mill" 0 11), 
                        (Customer 3 "Alice" 200 12, Customer 6 "Mill" 0 11), 
                        (Customer 6 "Mill" 0 11, Customer 3 "Alice" 200 12), 
                        (Customer 3 "Alice" 200 12, Customer 1 "John" 2000 10), 
                        (Customer 1 "John" 2000 10, Customer 2 "William" 3000 13), 
                        (Customer 0 "Mary" 5000 14, Customer 5 "Erica" 8000 16), 
                        (Customer 4 "William" 30 15, Customer 2 "William" 3000 13), 
                        (Customer 4 "William" 30 15, Customer 5 "Erica" 8000 16), 
                        (Customer 0 "Mary" 5000 14, Customer 0 "Mary" 5000 14), 
                        (Customer 1 "John" 2000 10, Customer 1 "John" 2000 10), 
                        (Customer 2 "William" 3000 13, Customer 2 "William" 3000 13), 
                        (Customer 3 "Alice" 200 12, Customer 3 "Alice" 200 12), 
                        (Customer 4 "William" 30 15, Customer 4 "William" 30 15), 
                        (Customer 5 "Erica" 8000 16, Customer 5 "Erica" 8000 16), 
                        (Customer 6 "Mill" 0 11, Customer 6 "Mill" 0 11), 
                        (Customer 7 "Bob" 9999 10, Customer 7 "Bob" 9999 10)]

-- Relational data:
locations = [Location 10 "Pietari Kalmin katu 5" "Helsinki" 00560 "Finland", 
                Location 11 "Lietaus g. 51" "Vilnius" 04231 "Lithuania", 
                Location 12 "Masterton Castlepoint Road" "Tinui" 5889 "New Zealand", 
                Location 13 "535 Pasir Ris Drive 1" "Northeast" 510535 "Singapore", 
                Location 14 "N 2" "Sandweiler" 5238 "Luxembourg", 
                Location 15 "Avenida Adolfo Eastman" "Olmue" 2330505 "Chile", 
                Location 16 "Industrivej 5" "Kjellerup" 8620 "Denmark"]

-- XML data:
products = [Product "2343f" "Toy" 66, 
                Product "3424g" "Book" 40, 
                Product "2543f" "Guitar" 668, 
                Product "1234r" "Carpet" 1, 
                Product "896h" "Jewelry" 5000, 
                Product "5698r" "Car" 9999, 
                Product "7890u" "Cup" 24, 
                Product "5467y" "Pen" 2] 

orders = [Order "34e5e79" [Product "2343f" "Toy" 66, Product "3424g" "Book" 40], 
            Order "0cbdf508" [Product"2543f" "Guitar" 668, Product "1234r" "Carpet" 1], 
            Order "4dwtfuu" [Product "2343f" "Toy" 66], 
            Order "3qqqeq9" [Product "2343f" "Toy" 66, Product "3424g" "Book" 40, Product "3424g" "Book" 40, Product "3424g" "Book" 40, Product "2543f" "Guitar" 668], 
            Order "77idy65" [Product "5467y" "Pen" 2, Product "5698r" "Car" 9999], 
            Order "ery63rg" [Product "7890u" "Cup" 24, Product "5467y" "Pen" 2, Product "3424g" "Book" 40, Product "2543f" "Guitar" 668, Product "896h" "Jewelry" 5000, Product "2343f" "Toy" 66]]

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

ordered :: Order -> Customer
ordered order = case orderNumber order of 
        "34e5e79" -> head $ foldg [] (\customer -> if customerId customer ==  1 then [customer] else []) (++) (++) customers
        "0cbdf508" -> head $ foldg [] (\customer -> if customerId customer ==  2 then [customer] else []) (++) (++) customers
        "4dwtfuu" -> head $ foldg [] (\customer -> if customerId customer ==  1 then [customer] else []) (++) (++) customers
        "3qqqeq9" -> head $ foldg [] (\customer -> if customerId customer ==  0 then [customer] else []) (++) (++) customers
        "77idy65" -> head $ foldg [] (\customer -> if customerId customer ==  3 then [customer] else []) (++) (++) customers
        "ery63rg" -> head $ foldg [] (\customer -> if customerId customer ==  5 then [customer] else []) (++) (++) customers 

-- For the following morphisms the first input is assumed to be fixed.
-- For each customer and for each order, there is a following kind of morphisms:

knows :: Customer -> Customer -> Bool
knows customer1 customer2 = hasEdge customer1 customer2 customers

located :: Customer -> Location
located customer = head (foldr (\x xs -> if locationId x == location customer then x:xs else xs) [] locations)

-- Now it follows that edgeInGraph2 customer1 customer2 customerId customerGraph == knows customer1 customer2

contains :: Order -> Product -> Bool
contains a b = elem b (orderProducts a)

-- Generally we can produce arbitrarily many morphims using the following function for each possible predicate in a query.

predicateMorphism :: a -> (a -> Bool) -> Bool
predicateMorphism a f = f a