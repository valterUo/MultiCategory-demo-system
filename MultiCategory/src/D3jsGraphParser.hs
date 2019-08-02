{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module D3jsGraphParser where

import Data.List
import GHC.Generics
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text)
import Debug.Trace

-- The datatypes that are standard for drawing graphs in D3.js

data Link = Link { source :: Int
    , target :: Int
} deriving (Show, Eq, Generic)

-- This datatype already contains encoded objects in nodes.

data D3jsGraph = D3jsGraph { nodes :: [Text]
    , links :: [Link]
    } deriving (Show, Eq, Generic)

instance ToJSON Link
instance ToJSON D3jsGraph

instance FromJSON Link
instance FromJSON D3jsGraph

encodeListToJSONText :: ToJSON a => [a] -> [Text]
encodeListToJSONText [] = []
encodeListToJSONText (x:xs) = (encodeToLazyText x) : (encodeListToJSONText xs)

findCustomer :: Eq b => [a] -> (a -> b) -> b -> Maybe a
findCustomer [] _ x = Nothing
findCustomer (x:xs) f a = if f x == a then Just x else findCustomer xs f a

findCustomerIndex :: (Eq a, Eq b) => [a] -> (a -> b) -> (b, b) -> Maybe Link
findCustomerIndex customers f (a, b) = case findCustomer customers f a of 
    Just sourceCustomer -> case findCustomer customers f b of 
        Just targetCustomer -> case elemIndex sourceCustomer customers of
            Just x -> case elemIndex targetCustomer customers of
                Just y -> Just(Link x y)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

constructLinks :: (Eq a, Eq b) => [a] -> (a -> b) -> [(b,b)] -> [Link]
constructLinks _ _ [] = []
constructLinks xs f (y:ys) = case findCustomerIndex xs f y of
    Just t -> t:(constructLinks xs f ys)
    Nothing -> constructLinks xs f ys

constructD3Graph :: (Eq a, Eq b, ToJSON a) => [a] -> (a -> b) -> [(b,b)] -> D3jsGraph
constructD3Graph xs f ys = D3jsGraph (encodeListToJSONText xs) (constructLinks xs f ys)

-- These functions are not in use at the moment:

populateGraphWithData :: [(a, a)] -> (a -> b) -> [(b,b)]
populateGraphWithData [] _ = []
populateGraphWithData ((x,y):xs) f = (f x, f y): populateGraphWithData xs f

restrictGraphWithData :: Eq a => [(a,a)] -> [a] -> [(a,a)]
restrictGraphWithData [] _ = []
restrictGraphWithData (x:xs) ys = let (a,b) = x in if elem a ys && elem b ys then x:restrictGraphWithData xs ys else restrictGraphWithData xs ys

checkGraphElementValues :: Eq b => b -> [a] -> (a -> b) -> Maybe a
checkGraphElementValues _ [] _ = Nothing
checkGraphElementValues x (y:ys) f = if f y == x then Just y else checkGraphElementValues x ys f

matchDataToGraph :: Eq b => [a] -> (a -> b) -> [(b,b)] -> [(a,a)]
matchDataToGraph _ _ [] = []
matchDataToGraph xs f (y:ys) = let (a,b) = y in
    case checkGraphElementValues a xs f of
        Nothing -> matchDataToGraph xs f ys
        Just t -> case checkGraphElementValues b xs f of
            Nothing -> matchDataToGraph xs f ys
            Just k -> (t,k):(matchDataToGraph xs f ys)