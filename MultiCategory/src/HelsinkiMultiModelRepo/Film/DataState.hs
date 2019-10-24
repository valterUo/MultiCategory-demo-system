module HelsinkiMultiModelRepo.Film.DataState where

import qualified Data.RDF as RDF
import HelsinkiMultiModelRepo.Film.SchemaCategory
import HelsinkiMultiModelRepo.Film.DataParser
import System.IO.Unsafe

films :: [Film]
films = unsafePerformIO $ collectFilms "HelsinkiMultiModelRepoDataSets\\filmDataSet\\film_imdb_listjson_lastversion.json"

filmGraph :: RDF.RDF RDF.TList
filmGraph = unsafePerformIO $ collectFilmGraph "HelsinkiMultiModelRepoDataSets\\filmDataSet\\smallMix.dbpedia.graph" --"HelsinkiMultiModelRepoDataSets\\filmDataSet\\film.dbpedia.graph"