{-# LANGUAGE DeriveGeneric     #-}

module HelsinkiMultiModelRepo.Patent.SchemaCategory where

import GHC.Generics
import Data.Aeson
import Data.Serialize
import qualified Data.IntMap.Strict as IntMap

-- Objects:

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
    , classCatId :: Int
    , classSubcatId :: Int 
    } deriving (Show, Eq, Generic)

data Patent = Patent { patentId :: Int
    , gyear :: Maybe Int
    , gdate :: Maybe Int
    , appyear :: Maybe Int
    , patentCountry :: String
    , patentPostate :: String
    , patentAssigneeId :: Maybe Int
    , asscode :: Maybe Int
    , claims :: Maybe Int
    , patentNclassId :: Maybe Int
    , patentCatId :: Maybe Int
    , patentSubcatId :: Maybe Int
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

data Inventor = Inventor { inventorPatentId :: Int
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

-- Some of the non-trivial morphisms:

patentAssignee :: Patent -> (IntMap.IntMap Assignee) -> Maybe Assignee
patentAssignee patent assingees = case patentAssigneeId patent of
    Just id -> IntMap.lookup id assingees
    Nothing -> Nothing

patentCat :: Patent -> (IntMap.IntMap Category) -> Maybe Category
patentCat patent categories = case patentCatId patent of
    Just id -> IntMap.lookup id categories
    Nothing -> Nothing

patentSubcat :: Patent -> (IntMap.IntMap Category) -> Maybe Category
patentSubcat patent categories = case patentSubcatId patent of
    Just id -> IntMap.lookup id categories
    Nothing -> Nothing

patentNclass :: Patent -> (IntMap.IntMap Class) -> Maybe Class
patentNclass patent classes = case patentNclassId patent of
    Just id -> IntMap.lookup id classes
    Nothing -> Nothing

classCat :: Class -> (IntMap.IntMap Category) -> Maybe Category
classCat classs categories = let id = classCatId classs in
    IntMap.lookup id categories

classSubcat :: Class -> (IntMap.IntMap Category) -> Maybe Category
classSubcat classs categories = let id = classSubcatId classs in
    IntMap.lookup id categories

inventorPatent :: Inventor -> (IntMap.IntMap Patent) -> Maybe Patent
inventorPatent inventor patents = let id = inventorPatentId inventor in 
    IntMap.lookup id patents