module HelsinkiMultiModelRepo.Film.InstanceCategory where

import qualified Data.RDF as RDF
import HelsinkiMultiModelRepo.Film.SchemaCategory
import HelsinkiMultiModelRepo.Film.DataParser
import System.IO.Unsafe

films :: [Film]
films = unsafePerformIO $ collectFilms "HelsinkiMultiModelRepoDataSets//filmDataSet//small_film_imdb.json"

filmGraph :: RDF.RDF RDF.TList
filmGraph = unsafePerformIO $ collectFilmGraph "HelsinkiMultiModelRepoDataSets//filmDataSet//smallMix.dbpedia.graph"