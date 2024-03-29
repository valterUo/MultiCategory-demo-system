{-# LANGUAGE OverloadedStrings #-}

module HelsinkiMultiModelRepo.Film.DataParser where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Internal as B
import Data.Aeson
import HelsinkiMultiModelRepo.Film.SchemaCategory
import qualified Data.RDF as RDF
import qualified Data.Text as T
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

createFilms :: [C.ByteString] -> IO([Film])
createFilms [] = return []
createFilms (x:xs) = case (decode(B.packChars(C.unpack x)) :: Maybe Film) of
    Nothing -> createFilms xs
    Just(film) -> do
        films <- createFilms xs 
        return $ (film):(films)

collectFilms :: FilePath -> IO([Film])
collectFilms path = do 
    content <- C.readFile path
    let linesOfFile = C.lines content in do
        result <- createFilms linesOfFile
        return result

-- Because Helsinki Multi-model repository does not contain RDF graphs that would follow any standart (N-triples, trutle, XML),
-- here is parser that parses special this case.

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

readRDF :: FilePath -> IO [[String]]
readRDF path = do str <- readFile path
                  return $ map (\line -> splitOnAnyOf [">", "<"] line) $ lines str

createTriple :: String -> String -> String -> RDF.Triple
createTriple x y z = RDF.Triple (RDF.UNode $ T.pack(x)) (RDF.UNode $ T.pack(y)) (RDF.UNode $ T.pack(z))

collectTriples :: [(String, String, String)] -> RDF.Triples
collectTriples [] = []
collectTriples ((x, y, z):xs) = (createTriple x y z) : collectTriples xs

createStringTuples :: [[String]] -> [(String, String, String)]
createStringTuples [] = []
createStringTuples (x:xs) = if length x > 4 then 
    let subject = (x !! 1) in
        let predicate = (x !! 3) in
            let object = (x !! 5) in
                if all (\x -> (x /= "" && x /= " " && x /= "\t")) [subject, object, predicate] then
                    (subject, predicate, object) : createStringTuples xs
                else createStringTuples xs
    else createStringTuples xs

collectFilmGraph :: FilePath -> IO(RDF.RDF RDF.TList)
collectFilmGraph filepath = do
    answer <- readRDF filepath 
    let graph = RDF.mkRdf (collectTriples $ createStringTuples $ answer) (Nothing) (RDF.PrefixMappings $ Map.fromList([])) in
        return (graph :: RDF.RDF RDF.TList)