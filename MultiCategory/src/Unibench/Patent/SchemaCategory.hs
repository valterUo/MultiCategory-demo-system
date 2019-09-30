{-# LANGUAGE DeriveGeneric     #-}

module Unibench.Patent.SchemaCategory where

import GHC.Generics
import Data.Aeson
import Data.Serialize

data Assignee = Assignee { assigneeId :: Int
    , assname :: String
    , pname :: String
    , sname :: String
    , own :: Maybe Int
    , cusip :: String
    , assigneeCname :: String
    } deriving (Show, Eq, Generic)

data Category = Category { catId :: Int
    , subcat :: Int
    , subcatcname :: String
    , catnameshort :: String
    , catnamelong :: String
    } deriving (Show, Eq, Generic)

data Class = Class { classId :: Int
    , cname :: String
    , classCat :: Maybe Category
    , classSubcat :: Maybe Category 
    } deriving (Show, Eq, Generic)

data Patent = Patent { patentId :: Int
    , gyear :: Maybe Int
    , gdate :: Maybe Int
    , appyear :: Maybe Int
    , patentCountry :: String
    , patentPostate :: String
    , patentAssignee :: Maybe Assignee
    , asscode :: Maybe Int
    , claims :: Maybe Int
    , patentNclass :: Maybe Class
    , patentCat :: Maybe Category
    , patentSubcat :: Maybe Category
    , cmade :: Maybe Int
    , creceive :: Maybe Int
    , ratiocit :: Maybe Int
    , general :: Maybe Int
    , original :: Maybe Int
    , fwdaplag :: Maybe Int
    , bckgtlag :: Maybe Int
    , selfctub :: Maybe Int
    , selfctlb :: Maybe Int
    , secdupbd :: Maybe Int
    , secdlwbd :: Maybe Int
    } deriving (Show, Eq, Generic)

data Inventor = Inventor { inventorPatent :: Maybe Patent
    , lastnam :: String
    , firstnam :: String
    , midnam :: String                     
    , modifnam :: String
    , street :: String
    , city :: String
    , inventorPostate :: String
    , inventorCountry :: String
    , zip :: String
    , invseq :: Maybe Int
    } deriving (Show, Eq, Generic)

instance ToJSON Assignee
instance ToJSON Inventor
instance ToJSON Patent
instance ToJSON Category
instance ToJSON Class
    
instance FromJSON Assignee
instance FromJSON Inventor
instance FromJSON Patent
instance FromJSON Category
instance FromJSON Class

instance Serialize Assignee
instance Serialize Inventor
instance Serialize Patent
instance Serialize Category
instance Serialize Class