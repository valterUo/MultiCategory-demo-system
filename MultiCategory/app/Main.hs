module Main where

import Data.List
import Data.ByteString.Lazy.Internal
import System.IO
import Control.Monad
import XMLParser
import SchemaCategory
import CSVParser
import Data.Aeson
import D3jsAlgebraicGraphParser
import qualified Data.Text.Lazy as L
import Algebra.Graph
import UnibenchPatentSchemaCategory
import UnibenchPatentDataParser
import QueryProcessing
import SimpleDemoDataState
import Data.IORef
import System.IO.Unsafe

encodeListToJSON :: ToJSON a => [a] -> [ByteString]
encodeListToJSON [] = []
encodeListToJSON (x:xs) = (encode x) : (encodeListToJSON xs)

wrapListToJSON :: ToJSON a => [a] -> String
wrapListToJSON xs = "{\"result\":[" ++ L.unpack( L.intercalate (L.pack ", ") (encodeListToJSONText xs)) ++ "]}"

main = undefined
    