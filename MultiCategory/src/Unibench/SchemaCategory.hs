{-# LANGUAGE DeriveGeneric     #-}

module Unibench.SchemaCategory where

import GHC.Generics
import Data.Aeson
import Algebra.Graph
import qualified Data.IntMap.Strict as IntMap

data Person = Person {
    personId :: Int,
    firstName :: String,
    lastName :: String,
    gender :: String,
    birthday :: String,
    personCreationDate :: String,
    personLocationIP :: String,
    personBrowserUsed :: String,
    place :: Int
} deriving (Show, Eq, Generic)

data Post = Post {
    postId :: Int,
    postCreationDate :: String,
    postLocationIP :: String,
    postBrowserUsed :: String,
    language :: String,
    content :: String,
    postLength :: Maybe Int
} deriving (Show, Eq, Generic)

data UnibenchProduct = UnibenchProduct {
    asin :: String,
    productTitle :: String,
    price :: Maybe Double,
    imgUrl :: String,
    unibenchProductId :: Int,
    brand :: Maybe Int
} deriving (Show, Eq, Generic)

data UnibenchOrder = UnibenchOrder {
    unibenchOrderId :: String,
    unibenchPersonId :: Int,
    orderData :: String,
    totalPrice :: Double,
    orderLine :: [UnibenchProduct]
} deriving (Show, Eq, Generic)