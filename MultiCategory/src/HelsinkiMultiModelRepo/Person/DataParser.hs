module HelsinkiMultiModelRepo.Person.DataParser where

import qualified Data.RDF as RDF
import Data.Maybe

collectPersonGraphMaybe :: String -> IO(Maybe (RDF.RDF RDF.TList))
collectPersonGraphMaybe path = do
    result <- RDF.parseFile RDF.NTriplesParser path
    case result of
        Right rdf -> return $ Just (rdf :: RDF.RDF RDF.TList)
        Left error -> return $ Nothing

collectPersonGraph :: String -> IO (RDF.RDF RDF.TList)
collectPersonGraph path = do
    result <- collectPersonGraphMaybe path
    return $ fromJust result