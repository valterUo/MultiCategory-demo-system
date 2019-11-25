{-# LANGUAGE DeriveGeneric     #-}

module Unibench.SchemaCategory where

import GHC.Generics
import Data.Aeson
import Algebra.Graph
import qualified Data.IntMap.Strict as IntMap

data Person = Person {
    personsId :: Int,
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

data Tag = Tag {
    tagId :: String,
    tagTitle :: String
} deriving (Show, Eq, Generic)

data UnibenchProduct = UnibenchProduct {
    productAsin :: String,
    productTitle :: String,
    productPrice :: Maybe Double,
    imgUrl :: String,
    unibenchProductId :: Int,
    productBrand :: Maybe Int
} deriving (Show, Eq, Generic)

data UnibenchOrderline = UnibenchOrderline {
    productId :: String,
    asin :: String,
    title :: String,
    price :: Double,
    brand :: String
} deriving (Show, Eq, Generic)

data UnibenchOrder = UnibenchOrder {
    orderid :: String,
    personid :: String,
    orderdate :: String,
    totalprice :: Double,
    orderline :: [UnibenchOrderline]
} deriving (Show, Eq, Generic)

data Invoice = Invoice {
    order :: UnibenchOrder
} deriving (Show, Eq, Generic)

data Feedback = Feedback {
    feedbackAsin :: String,
    feedbackPersonId :: Int,
    feedback :: String
} deriving (Show, Eq, Generic)

data Vendor = Vendor {
    vendor :: String,
    vendorCountry :: String,
    vendorIndustry :: String
} deriving (Show, Eq, Generic)

instance ToJSON Person
instance ToJSON Post
instance ToJSON Tag
instance ToJSON UnibenchProduct
instance ToJSON UnibenchOrder
instance ToJSON Invoice
instance ToJSON Feedback
instance ToJSON Vendor
instance ToJSON UnibenchOrderline

instance FromJSON Person
instance FromJSON Post
instance FromJSON Tag
instance FromJSON UnibenchProduct
instance FromJSON UnibenchOrder
instance FromJSON Invoice
instance FromJSON Feedback
instance FromJSON Vendor
instance FromJSON UnibenchOrderline