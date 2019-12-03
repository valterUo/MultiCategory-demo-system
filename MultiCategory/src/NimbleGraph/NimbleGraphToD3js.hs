{-# LANGUAGE DeriveGeneric     #-}

module NimbleGraph.NimbleGraphToD3js where

import GHC.Generics
import Data.Aeson
import Data.HashMap.Strict
import D3jsAlgebraicGraphParser
import NimbleGraph.NimbleGraph
import Data.List
import Data.Text.Lazy (Text)
import Data.Aeson.Text (encodeToLazyText)

createNimbleNodes :: ToJSON a => HashMap String (NimbleVertex a) -> [Text]
createNimbleNodes vertexMap = Data.HashMap.Strict.foldr (\x xs ->  encodeToLazyText (vertexValue x):xs) [] vertexMap

createNimbleEdges :: (ToJSON a, Eq a, ToJSON b, Show b) => HashMap String (NimbleEdge a b) -> [Text] -> [Link]
createNimbleEdges edgesMap nodes = Data.HashMap.Strict.foldr (\x xs -> let sourceVertex = vertexValue(NimbleGraph.NimbleGraph.source x) in
    let targetVertex = vertexValue(NimbleGraph.NimbleGraph.target x) in
        case elemIndex (encodeToLazyText sourceVertex) nodes of
            (Just i) -> case elemIndex (encodeToLazyText targetVertex) nodes of
                            (Just j) -> (Link i j (show (edgeValue x))):xs
                            Nothing -> xs
            Nothing -> xs ) [] edgesMap

createD3NimbleGraph :: (ToJSON a, Eq a, ToJSON b, Show b) => NimbleGraph a b -> D3jsGraph
createD3NimbleGraph graph = let nodes = createNimbleNodes (vertices graph) in 
    D3jsGraph (nodes) (createNimbleEdges (edges graph) nodes)