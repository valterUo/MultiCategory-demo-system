module XMLParser where
    
import Data.List
import System.IO
import Control.Monad
import Xeno.DOM
import Xeno.Types
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import SchemaCategory

-- XML part of the demo system backend:
-- These functions work generally for any XML tree structure:

parseXMLString :: String -> Either XenoException Node
parseXMLString xs = parse(C8.pack xs)
    
nodeIsLeaf :: Node -> Bool
nodeIsLeaf node = if length(children(node)) == 0 then True else False
    
allChildrenLeaves :: [Node] -> Bool
allChildrenLeaves [] = False
allChildrenLeaves [x] = nodeIsLeaf x
allChildrenLeaves (x:xs) = if nodeIsLeaf x then allChildrenLeaves xs else False

unwrapLeafContent :: [Content] -> Maybe String
unwrapLeafContent [] = Nothing
unwrapLeafContent xs = if length xs > 1
    then Nothing 
    else let first = head xs in 
    case first of
        Text x -> Just (C8.unpack x)
        CData x -> Just (C8.unpack x)

collectDataFromLeaves :: [Node] -> [String]
collectDataFromLeaves [] = []
collectDataFromLeaves (x:xs) = let Just content = unwrapLeafContent(contents(x)) in content : collectDataFromLeaves xs

addListAsSet :: Eq a => a -> [a] -> [a]
addListAsSet x xs = if elem x xs then xs else x:xs

addListToListAsSet :: Eq a => [a] -> [a] -> [a]
addListToListAsSet xs [] = xs
addListToListAsSet [] ys = ys
addListToListAsSet (x:xs) ys = addListToListAsSet xs (addListAsSet x ys) 

-- The following functions depend on the example data. 

collectProduct :: Node -> Maybe Product
collectProduct node = if allChildrenLeaves (children node)
    then let content = collectDataFromLeaves(children(node)) in Just (Product (content !! 0) (content !! 1) (read (content !! 2) :: Int))
    else Nothing

collectProducts :: [Node] -> Maybe [Product]
collectProducts [] = Just []
collectProducts (n:nodes) = case collectProduct n of
    Nothing -> Nothing
    Just x -> let Just tailProducts = collectProducts nodes in Just (x : tailProducts)

collectOrder :: Node -> Order
collectOrder node = let firstChild = head (children node) in
    let Just products = collectProducts (tail (children node)) in
    let Just orderNum = unwrapLeafContent(contents firstChild) in
    Order (orderNum) (products)

collectOrders :: [Node] -> [Order]
collectOrders [] = []
collectOrders (order:orders) = (collectOrder order) : (collectOrders orders)

xmlTreeToList :: Node -> [Order]
xmlTreeToList node = collectOrders(children node)

readXMLFile :: FilePath -> IO [Order]
readXMLFile path = do
    xmlData <- readFile path
    let Right node = parse(C8.pack xmlData) in return $ xmlTreeToList node

returnProducts :: [Order] -> [Product]
returnProducts [] = []
returnProducts (order:orders) = addListToListAsSet (orderProducts order) (returnProducts orders) 