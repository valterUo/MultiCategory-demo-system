module HelsinkiMultiModelRepo.University.DataParser where

import qualified Data.IntMap.Strict as IntMap
import HelsinkiMultiModelRepo.University.SchemaCategory
import CSVParser

createUniversitites :: [[String]] -> [(Int, University)]
createUniversitites [] = []
createUniversitites (x:xs) = ((read(x !! 0) :: Int), (University (read(x !! 0) :: Int) 
                                                                 (x !! 1)
                                                                 (x !! 2) 
                                                                 (x !! 3)  
                                                                 (x !! 4)
                                                                 (x !! 5)
                                                                 (x !! 6) 
                                                                 (x !! 7)  
                                                                 (x !! 8)
                                                                 (x !! 9)
                                                                 (x !! 10) 
                                                                 (x !! 11)  
                                                                 (x !! 12)
                                                                 (x !! 13)
                                                                 (x !! 14) 
                                                                 (x !! 15)  
                                                                 (x !! 16)
                                                                 (x !! 17)
                                                                 (x !! 18) 
                                                                 (x !! 19)  
                                                                 (x !! 20)
                                                                 (x !! 21)
                                                                 (x !! 22))): createUniversitites xs

collectUniversities :: FilePath -> IO (IntMap.IntMap University)
collectUniversities path = do
    result <- readCSV "," path
    return $ IntMap.fromList $ createUniversitites $ tail result