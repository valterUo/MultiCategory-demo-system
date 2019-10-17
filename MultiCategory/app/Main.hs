{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.List
import GHC.Generics
import Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString.Lazy as D
import System.IO
import Control.Monad
import XMLParser
import SchemaCategory
import CSVParser
import Data.Aeson
import D3jsAlgebraicGraphParser
import qualified Data.Text.Lazy as L
import Algebra.Graph
import HelsinkiMultiModelRepo.Patent.SchemaCategory
import HelsinkiMultiModelRepo.Patent.DataParser
import QueryProcessing
import SimpleDemoDataState
import Data.IORef
import System.IO.Unsafe
import qualified Data.Serialize as S
import HelsinkiMultiModelRepo.Film.SchemaCategory
import HelsinkiMultiModelRepo.Film.DataParser
import Data.Typeable 
import Data.Data
import GraphFunctions
import Data.RDF as RDF
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

encodeListToJSON :: ToJSON a => [a] -> [B.ByteString]
encodeListToJSON [] = []
encodeListToJSON (x:xs) = (encode x) : (encodeListToJSON xs)

wrapListToJSON :: ToJSON a => [a] -> String
wrapListToJSON xs = "{\"result\":[" ++ L.unpack( L.intercalate (L.pack ", ") (encodeListToJSONText xs)) ++ "]}"

main = do
    -- case parseString NTriplesParser (T.pack("<AAAband> <activeYearsStartYear> \"2005\"^^<http://www.w3.org/2001/XMLSchema#gYear> .")) of
    --     Right rdf -> print $ RDF.showGraph $ (rdf :: RDF TList)
    --     Left error -> print error

    answer <- readRDF "UnibenchData\\filmDataSets\\smallMix.dbpedia.graph"
    --print((answer !! 1) ++ [" "] ++ (answer !! 3) ++ [" "] ++ (answer !! 5))
    let graph = RDF.mkRdf (collectTriples $ createStringTuples $ answer) (Nothing) (RDF.PrefixMappings $ Map.fromList([])) in
         print $ foldrdf (\x xs -> if length xs > 20 then xs else (predicateOf x : xs)) [] (graph :: RDF TList)
    