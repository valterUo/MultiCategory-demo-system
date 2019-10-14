{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.List
import GHC.Generics
import Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString.Lazy as D
import System.IO
import Control.Monad
import XMLParser
import SchemaCategory
import CSVParser
import Data.Aeson
import D3jsAlgebraicGraphParser
import qualified Data.Text.Lazy as L
import Algebra.Graph
import HelsinkiMultiModelRepo.Patent.SchemaCategory
import HelsinkiMultiModelRepo.Patent.DataParser
import QueryProcessing
import SimpleDemoDataState
import Data.IORef
import System.IO.Unsafe
import qualified Data.Serialize as S
import qualified Data.Map.Lazy as LazyMap
import HelsinkiMultiModelRepo.Film.SchemaCategory
import Data.Typeable 
import Data.Data
import GraphFunctions

encodeListToJSON :: ToJSON a => [a] -> [B.ByteString]
encodeListToJSON [] = []
encodeListToJSON (x:xs) = (encode x) : (encodeListToJSON xs)

wrapListToJSON :: ToJSON a => [a] -> String
wrapListToJSON xs = "{\"result\":[" ++ L.unpack( L.intercalate (L.pack ", ") (encodeListToJSONText xs)) ++ "]}"

main = do
    let graphLeft = customers in
        let y = (Customer 5 "Erica" 8000 16) in
            let x = (Customer 1 "John" 2000 10) in
                if x == y then print True else
                    let neighbors = removeVertex x (findTargetNeighbors x graphLeft) in do
                        print "Neighbors: \n"
                        print neighbors
                        print "\n"
                        if isEmpty(graphLeft) || isEmpty(neighbors) then print False else 
                                if hasVertex y neighbors then print True else
                                    let targetList = nub $ (foldg [] (\x -> [x]) (++) (++) neighbors) in do
                                        print "Targetlist: \n"
                                        print targetList
                                        print "\n"
                                        let newGraph = removeVertex x graphLeft in do
                                            print "NewGraph: \n"
                                            print newGraph
                                            print "\n"
                                            if isEmpty(newGraph) then print False else
                                                let booleanList = map (\z -> reachable z y newGraph) targetList in do
                                                    print y
                                                    print targetList
                                                    print $ booleanList
    --h <- openFile "demoData\\newfile.txt" ReadWriteMode
    --print $ S.encodeLazy (Assignee 16716 "fadf" "fdasfdsa" "fdsafdsa" (Just 67463) "fadf" "fdasfda")
    -- r <- D.writeFile "demoData\\newfile" (S.encodeLazy (Assignee 16716 "fadf" "fdasfdsa" "fdsafdsa" (Just 67463) "fadf" "fdasfda"))
    -- print "End"
    -- print r
    -- t <- C.readFile "demoData\\newfile"
    -- print $ (S.decode t :: Either String Assignee)

    -- let result = (decode("{\"title\": \"Electric Shadows\", \"year\": \"2004\", \"rated\": \"N/A\", \"released\": \"07 Apr 2005\", \"runtime\": \"93 min\", \"genre\": \"Drama\", \"director\": \"Jiang Xiao\", \"writer\": \"Qingsong Cheng, Jiang Xiao\", \"actors\": \"Yu Xia, Haibin Li, Yijing Zhang, Zhongyang Qi\", \"plot\": \"For no apparent reason, a mute young woman assaults a youth who delivers water on his bicycle, injuring him and ruining his bike. Surprisingly, she asks him to feed her fish while she is in custody. Her tiny apartment, he discovers, is a shrine to his favorite escape, the movies. He finds her diary - a screenplay of her life built around scenes from favorite films - and it sets off his imagination. Maybe they have more in common than a love of the movies.\", \"language\": \"Mandarin\", \"country\": \"China\", \"awards\": \"4 wins & 1 nomination.\", \"poster\": \"http://ia.media-imdb.com/images/M/MV5BMjE1NTg5MzY4MV5BMl5BanBnXkFtZTcwMDY5OTMzMQ@@._V1_SX300.jpg\", \"metascore\": \"70\", \"imdbrating\": \"7.6\", \"imdbvotes\": \"869\", \"imdbid\": \"tt0424273\", \"typeClass\": \"movie\", \"response\": \"True\"}") :: Maybe Film) in
    --     return result 

    -- reachable (Customer 1 "John" 2000 10) (Customer 3 "Alice" 200 12) customers
    -- reachable (Customer 6 "Mill" 0 11) (Customer 5 "Erica" 8000 16) customers
    -- reachable (Customer 2 "William" 3000 13) (Customer 5 "Erica" 8000 16) customers
    -- reachable (Customer 5 "Erica" 8000 16) (Customer 1 "John" 2000 10) customers
    -- findTargetNeighbors findTargetNeighborsWithEmptyNodes (Customer 1 "John" 2000 10) customers
    -- findTargetNeighborsWithEmptyNodes (Customer 2 "William" 3000 13)
    