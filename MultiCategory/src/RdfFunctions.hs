module RdfFunctions where

import Data.RDF
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import D3jsAlgebraicGraphParser
import Data.List
--import qualified Data.Set.StringSet as Set

-- Specialized folding for RDF graphs based on their list presentation

foldrdf ::  Rdf a => (Triple -> b -> b) -> b -> RDF a -> b
foldrdf f z rdf = foldr f z (triplesOf rdf)

rdfContainsTriple :: Rdf a => RDF a -> Maybe String -> Maybe String -> Maybe String -> Triples
rdfContainsTriple graph Nothing Nothing Nothing = triplesOf graph
rdfContainsTriple graph Nothing (Just y) (Just z) = select graph Nothing (Just(\a -> a == UNode(T.pack(y)))) (Just(\a -> a == UNode(T.pack(z))))
rdfContainsTriple graph (Just x) Nothing (Just z) = select graph (Just(\a -> a == UNode(T.pack(x)))) Nothing (Just(\a -> a == UNode(T.pack(z))))
rdfContainsTriple graph (Just x) (Just y) Nothing = select graph (Just(\a -> a == UNode(T.pack(x)))) (Just(\a -> a == UNode(T.pack(y)))) Nothing
rdfContainsTriple graph (Just x) Nothing Nothing = select graph (Just(\a -> a == UNode(T.pack(x)))) Nothing Nothing
rdfContainsTriple graph Nothing (Just y) Nothing = select graph Nothing (Just(\a -> a == UNode(T.pack(y)))) Nothing
rdfContainsTriple graph Nothing Nothing (Just z) = select graph Nothing Nothing (Just(\a -> a == UNode(T.pack(z))))
rdfContainsTriple graph (Just x) (Just y) (Just z) = select graph (Just(\a -> a == UNode(T.pack(x)))) (Just(\a -> a == UNode(T.pack(y)))) (Just(\a -> a == UNode(T.pack(z))))

collectRdfNodes :: Triples -> ([L.Text], [Link])
collectRdfNodes [] = ([], [])
collectRdfNodes (triple:triples) = do
    let (nodes, links) = collectRdfNodes triples in
        let Triple a b c = triple in
            if (isUNode a && isUNode b && isUNode c) then 
                let UNode value3 = a in
                    let UNode value4 = c in
                        let UNode linkName = b in
                            let value1 = (L.pack("{ \"name\":" ++ "\"" ++ (T.unpack value3) ++ "\"}")) in
                                let value2 = (L.pack("{ \"name\":" ++ "\"" ++ (T.unpack value4) ++ "\"}")) in
                                    case elemIndex value1 nodes of
                                        Nothing -> case elemIndex value2 nodes of
                                            Nothing -> let source = length nodes in
                                                let target = length nodes + 1 in
                                                    let link = Link source target (T.unpack linkName) in
                                                        ((value2:value1:nodes), (link:links))
                                            Just j -> let source = length nodes in
                                                let target = j in
                                                    let link = Link source target (T.unpack linkName) in
                                                        ((value1:nodes), (link:links))
                                        Just i -> case elemIndex value2 nodes of
                                            Nothing -> let source = i in
                                                let target = length nodes in
                                                    let link = Link source target (T.unpack linkName) in
                                                        ((value2:nodes), (link:links))
                                            Just j -> let source = i in
                                                let target = j in
                                                    let link = Link source target (T.unpack linkName) in
                                                        (nodes, (link:links))
            else (nodes, links)

rdfTriplesToD3Graph :: Triples -> D3jsGraph
rdfTriplesToD3Graph triples = let (nodes, links) = collectRdfNodes triples in D3jsGraph nodes links