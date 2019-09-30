{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- Another datatypes arise from RDF graph which forms a challenge at the moment.
-- Most probably RDF graph is a collection type and inside the graph we have just three types of objects: subject, predicate and object.
-- This allows us to handle large variety of the data and store lot of different things there. On the other, structure does not reveal so much
-- about the inner structure of the graph and querying will be slower.

module Unibench.Film.SchemaCategory where

import GHC.Generics
import Data.Aeson
import Data.Serialize
import Data.Typeable 
import Data.Data

data Film = Film { title :: String
    , year :: String
    , rated :: String
    , released :: String
    , runtime :: String
    , genre :: String
    , director :: String
    , writer :: String
    , actors :: String
    , plot :: String
    , language :: String
    , country :: String
    , awards :: String
    , poster :: String
    , metascore :: String
    , imdbrating :: String
    , imdbvotes :: String
    , imdbid :: String
    , typeClass :: String
    , response :: String
    } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Film
instance FromJSON Film
instance Serialize Film