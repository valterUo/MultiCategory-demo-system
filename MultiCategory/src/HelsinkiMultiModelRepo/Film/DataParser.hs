module HelsinkiMultiModelRepo.Film.DataParser where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Internal as B
import Data.Aeson
import HelsinkiMultiModelRepo.Film.SchemaCategory
import Data.RDF as RDF
import qualified Data.Text as T

createFilms :: [C.ByteString] -> IO([Film])
createFilms [] = return []
createFilms (x:xs) = case (decode(B.packChars(C.unpack x)) :: Maybe Film) of
    Nothing -> createFilms xs
    Just(film) -> do
        films <- createFilms xs 
        return $ (film):(films)

collectFilms :: FilePath -> IO([Film])
collectFilms path = do 
    content <- C.readFile path --"D:\\Film_dataset\\Film_dataset\\processed_dataset\\film_imdb_listjson.json"
    let linesOfFile = C.lines content in do
        print $ linesOfFile !! 0
        result <- createFilms linesOfFile
        return result

createTriple :: String -> String -> String -> RDF.Triple
createTriple x y z = RDF.Triple (RDF.UNode $ T.pack(x)) (RDF.UNode $ T.pack(y)) (RDF.UNode $ T.pack(z))

collectTriple :: [(String, String, String)] -> RDF.Triples
collectTriple [] = []
collectTriple ((x, y, z):xs) = (createTriple x y z) : collectTriple xs