module RdfFunctions where

import Data.RDF
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import D3jsAlgebraicGraphParser
import Data.List
import Data.Maybe

--Parsing any file that contains Ntriples

collectRDFMaybe :: String -> IO(Maybe (RDF TList))
collectRDFMaybe path = do
    result <- parseFile NTriplesParser path
    case result of
        Right rdf -> return $ Just (rdf :: RDF TList)
        Left error -> return $ Nothing

collectRDF :: String -> IO (RDF TList)
collectRDF path = do
    result <- collectRDFMaybe path
    return $ fromJust result

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

tripleContainsString :: Triple -> Maybe String -> Maybe String -> Maybe String -> Bool
tripleContainsString triple x y z = let (Triple a b c) = triple in 
    nodeContainsString a x && nodeContainsString b y && nodeContainsString c z

nodeContainsString :: Node -> Maybe String -> Bool
nodeContainsString _ Nothing = True
nodeContainsString node (Just a) = if isUNode node then let UNode txt = node in 
    isInfixOf a (T.unpack txt) else 
        if isLNode node then let LNode value = node in 
            isInfixOf a (show value) else 
                if isBNode node then let BNode txt = node in 
                    isInfixOf a (T.unpack txt) else 
                        False

repl :: Char -> Char
repl '"' = '.'
repl '\\' = '.'
repl c = c

collectRdfNodes :: Triples -> ([L.Text], [Link])
collectRdfNodes [] = ([], [])
collectRdfNodes (triple:triples) = do
    let (nodes, links) = collectRdfNodes triples in
        let Triple a b c = triple in
            let value1 = (L.pack("{ \"name\": \"" ++ (map repl (show a)) ++ "\"}")) in
                let value2 = (L.pack("{ \"name\":\"" ++ (map repl (show c)) ++ "\"}")) in
                    case elemIndex value1 nodes of
                        Nothing -> case elemIndex value2 nodes of
                            Nothing -> let source = length nodes in
                                let target = length nodes + 1 in
                                    let link = Link source target (show b) in
                                        (((nodes ++ [value1]) ++ [value2]), (link:links))
                            Just j -> let target = length nodes in
                                let source = j in
                                    let link = Link source target (show b) in
                                        ((nodes ++ [value1]), (link:links))
                        Just i -> case elemIndex value2 nodes of
                            Nothing -> let source = i in
                                let target = length nodes in
                                    let link = Link source target (show b) in
                                        ((nodes ++ [value2]), (link:links))
                            Just j -> let source = i in
                                let target = j in
                                    let link = Link source target (show b) in
                                        (nodes, (link:links))

rdfTriplesToD3Graph :: Triples -> D3jsGraph
rdfTriplesToD3Graph triples = let (nodes, links) = collectRdfNodes triples in D3jsGraph nodes links