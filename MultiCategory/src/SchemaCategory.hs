{-# LANGUAGE DeriveGeneric     #-}

module SchemaCategory where

import GHC.Generics
import Data.Aeson

-- Old style: Data is just in global variables.
-- SQL data:
customers = [Customer 0 "Mary" 5000 14, Customer 1 "John" 2000 10, Customer 2 "William" 3000 13, Customer 3 "Alice" 200 12, Customer 4 "William" 30 15, Customer 5 "Erica" 8000 16, Customer 6 "Mill" 0 11, Customer 7 "Bob" 9999 10]

-- Graph data: Possibly user wants to add (0,0), (1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7)
customerGraph = [(1,6), (3,6), (6,3), (3,1), (1,2), (0,5), (4,2), (4,5), (0,0), (1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7)]

-- XML data:
products = [Product "2343f" "Toy" 66, Product "3424g" "Book" 40, Product "2543f" "Guitar" 668, Product "1234r" "Carpet" 1, Product "896h" "Jewelry" 5000, Product "5698r" "Car" 9999, Product "7890u" "Cup" 24, Product "5467y" "Pen" 2] 
orders = [Order "34e5e79" [Product "2343f" "Toy" 66, Product "3424g" "Book" 40], Order "0cbdf508" [Product"2543f" "Guitar" 668, Product "1234r" "Carpet" 1], Order "4dwtfuu" [Product "2343f" "Toy" 66], Order "3qqqeq9" [Product "2343f" "Toy" 66, Product "3424g" "Book" 40, Product "3424g" "Book" 40, Product "3424g" "Book" 40, Product "2543f" "Guitar" 668], Order "77idy65" [Product "5467y" "Pen" 2, Product "5698r" "Car" 9999], Order "ery63rg" [Product "7890u" "Cup" 24, Product "5467y" "Pen" 2, Product "3424g" "Book" 40, Product "2543f" "Guitar" 668, Product "896h" "Jewelry" 5000, Product "2343f" "Toy" 66]]

-- Defining datatypes, we assume that we have types String, Int and Bool.
-- The following construction follows the definition of the category Hask.
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
    , city :: String
    , zipCode :: Int
    , country :: String
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
ordered :: Order -> [Customer] -> Customer
ordered x customers = case orderNumber x of 
    "34e5e79" -> customers !! 1
    "0cbdf508" -> customers !! 2
    "4dwtfuu" -> customers !! 1
    "3qqqeq9" -> customers !! 0
    "77idy65" -> customers !! 3
    "ery63rg" -> customers !! 5

-- For the following morphisms the first input is assumed to be fixed.
-- WE NEED TO HAVE A WAY TO TRANSFORM KEY VALUE DATA INTO HASKELL FUNCTIONS. THESE ARE SOME OF THE MORPHISMS.
-- For each customer and for each order, there is a following kind of morphisms:

knows :: Customer -> Customer -> Bool
knows customer1 customer2 = elem (customerId customer1, customerId customer2) customerGraph ||  elem (customerId customer2, customerId customer1) customerGraph

-- More general functions for handeling graphs

edgeInGraph :: Eq a => a -> a -> [(a,a)] -> Bool
edgeInGraph x y xs = elem (x,y) xs

edgeInGraph2 :: Eq a => a -> a -> (a -> Int) -> [(Int,Int)] -> Bool
edgeInGraph2 x y f xs = elem (f x, f y) xs || elem (f y, f x) xs

-- Now it follows that edgeInGraph2 customer1 customer2 customerId customerGraph == knows customer1 customer2

contains :: Order -> Product -> Bool
contains a b = elem b (orderProducts a)

-- Generally we can produce arbitrarily many morphims using the following function for each possible predicate in a query.

predicateMorphism :: a -> (a -> Bool) -> Bool
predicateMorphism a f = f a

-- It is clear that we can compose these functions naturally as functions are composed in Haskell. Composition of two parameters functions when another parameter is fized, 
-- is not by default supported by Haskell but we can extend the composition ".".