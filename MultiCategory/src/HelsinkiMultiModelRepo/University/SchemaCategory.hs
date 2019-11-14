{-# LANGUAGE DeriveGeneric     #-}

module HelsinkiMultiModelRepo.University.SchemaCategory where

import GHC.Generics
import Data.Aeson
import Data.Serialize
import qualified Data.IntMap.Strict as IntMap

-- Objects:

data University = University { 
    unitid :: Int,
    opeid :: String,
    opeid6 :: String,
    instnm :: String,
    uniCity :: String,
    stabbr :: String,
    uniZip :: String,
    accredAgency :: String,
    insturl :: String,
    npcurl :: String,
    sch_deg :: String,
    hcm2 :: String,
    main :: String,
    numbranch :: String,
    preddeg :: String,
    highdeg :: String,
    control :: String,
    st_fips :: String,
    region :: String,
    locale :: String,
    locale2 :: String,
    latitude :: String,
    longitude :: String
} deriving (Show, Eq, Generic)

instance ToJSON University
instance FromJSON University