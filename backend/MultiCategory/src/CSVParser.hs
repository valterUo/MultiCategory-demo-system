module CSVParser where

-- General CSV file parser

import Data.List.Split

readCSV :: String -> FilePath -> IO [[String]]
readCSV delimiter path = do 
  str <- readFile path
  return $ map (\x -> splitOn delimiter x) (lines str)