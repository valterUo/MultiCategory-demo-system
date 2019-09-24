module CSVParser where

-- General CSV file parser

readCSV :: FilePath -> IO [[String]]
readCSV path = do str <- readFile path
                  return $ map process $ lines str
  where process xs = case break (==';') xs of (a,[])    -> [a]
                                              (a,';':b) -> a:process b