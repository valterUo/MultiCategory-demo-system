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
import Data.Aeson
import D3jsAlgebraicGraphParser
import qualified Data.Text.Lazy as L
import Algebra.Graph
import Data.IORef
import System.IO.Unsafe
import qualified Data.Serialize as S
import Data.Typeable 
import Data.Data
import GraphFunctions
import Data.RDF as RDF
import qualified Data.Text as T
import Unibench.DataParser
import Unibench.SchemaCategory
import NimbleGraph.NimbleGraph
import NimbleGraph.NimbleGraphToD3js
import qualified Data.HashMap.Strict as Map

encodeListToJSON :: ToJSON a => [a] -> [B.ByteString]
encodeListToJSON [] = []
encodeListToJSON (x:xs) = (encode x) : (encodeListToJSON xs)

wrapListToJSON :: ToJSON a => [a] -> String
wrapListToJSON xs = "{\"result\":[" ++ L.unpack( L.intercalate (L.pack ", ") (encodeListToJSONText xs)) ++ "]}"

main = undefined --do
    -- print $ encode $ createD3NimbleGraph $ foldNimble (\vertex newGraph -> let 
    --     person = vertexValue vertex in 
    --         if firstName person == "Li" && lastName person == "Li" 
    --             then addVertex vertex newGraph else newGraph) (\edge newGraph -> case (Map.lookup (vertexId $ NimbleGraph.NimbleGraph.source edge) (NimbleGraph.NimbleGraph.vertices newGraph)) of Nothing -> newGraph; Just(sourceVertex) -> case Map.lookup (vertexId $ NimbleGraph.NimbleGraph.target edge) (NimbleGraph.NimbleGraph.vertices newGraph) of Nothing -> newGraph; Just(targetVertex) -> addEdge edge newGraph) emptyNimbleGraph personKnowsPersonGraph

    -- print $ (decode(B.packChars "{\"orderid\": \"016f6a4a-ec18-4885-b1c7-9bf2306c76d6\", \"personid\": \"10995116278711\", \"orderdate\": \"2022-09-01\", \"totalprice\": 723.88, \"orderline\": [{\"productId\": \"6465\", \"asin\": \"B000FIE4WC\", \"title\": \"Topeak Dual Touch Bike Storage Stand\", \"price\": 199.95, \"brand\": \"MYLAPS_Sports_Timing\"}, {\"productId\": \"178\", \"asin\": \"B002Q6DB7A\", \"title\": \"Radians Eclipse RXT Photochromic Lens with Black Frame Glass\", \"price\": 61.99, \"brand\": \"Elfin_Sports_Cars\"}, {\"productId\": \"6427\", \"asin\": \"B000SE9LDK\", \"title\": \"Sportlock Leatherlock Series Deluxe Take-Down Shotgun Case\", \"price\": 84.99, \"brand\": \"MYLAPS_Sports_Timing\"}, {\"productId\": \"7570\", \"asin\": \"B005G2G2OU\", \"title\": \"ESEE-5 Serr Olive Drab Textured Poweder Coated Blade Drop Point Style 1095 Carbon Steel-57 Rc\", \"price\": 172.95, \"brand\": \"Derbi\"}, {\"productId\": \"1991\", \"asin\": \"B00245TWWG\", \"title\": \"Marcy Classic MD 859P Mid Size Bench\", \"price\": 204.0, \"brand\": \"CCM_(ice_hockey)\"}]}") :: Maybe UnibenchOrder)
    -- result <- collectUnibenchOrders "D:\\Unibench-0.1\\Dataset\\Order\\new_order.json"
    -- print result

    -- result <- parseFile NTriplesParser "D:\\Helsinki Multi-Model Datasets\\Person_dataset\\Person_dataset\\reprocessed_dataset\\small_new_personGraph.graph"
    -- case result of
    --     Right rdf -> print $ RDF.showGraph $ (rdf :: RDF TList)
    --     Left error -> print error

    -- answer <- readRDF "UnibenchData\\filmDataSets\\smallMix.dbpedia.graph"
    -- print((answer !! 1) ++ [" "] ++ (answer !! 3) ++ [" "] ++ (answer !! 5))
    -- let graph = RDF.mkRdf (collectTriples $ createStringTuples $ answer) (Nothing) (RDF.PrefixMappings $ Map.fromList([])) in
    --     print $ foldrdf (\x xs -> if length xs > 20 then xs else (predicateOf x : xs)) [] (graph :: RDF TList)

    -- print $ foldNimble (\v g -> case unwrapEitherVertex v of 
    --     Right(personVertex) -> nimbleGraphUnion (inComingNeighbors personVertex personKnowsPersonGraph) g
    --     Left(postVertex) -> g) (\edge newGraph -> case (Map.lookup (vertexId $ NimbleGraph.NimbleGraph.source edge) (NimbleGraph.NimbleGraph.vertices newGraph)) of 
    --                             Nothing -> newGraph 
    --                             Just(sourceVertex) -> case Map.lookup (vertexId $ NimbleGraph.NimbleGraph.target edge) (NimbleGraph.NimbleGraph.vertices newGraph) of 
    --                                                 Nothing -> newGraph 
    --                                                 Just(targetVertex) -> addEdge edge newGraph) emptyNimbleGraph personCreatedPostGraph
    