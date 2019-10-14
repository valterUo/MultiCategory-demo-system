module UnibeHelsinkiMultiModelReponch.Film.DataParser where

import qualified Data.ByteString.Char8 as C
import Data.Aeson
import HelsinkiMultiModelRepo.Film.SchemaCategory

createFilms :: [C.ByteString] -> IO([Film])
createFilms [] = return []
createFilms (x:xs) = case (decode(packChars(C.unpack x)) :: Maybe Film) of
    Nothing -> createFilms xs
    Just(film) -> do
        films <- createFilms xs 
        return $ (film):(films)

collectFilms :: FilePath -> IO([Film])
collectFilms path = content <- C.readFile path --"D:\\Film_dataset\\Film_dataset\\processed_dataset\\film_imdb_listjson.json"
let linesOfFile = C.lines content in do
    print $ linesOfFile !! 0
    result <- createFilms linesOfFile
    return result