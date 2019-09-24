module UnibenchPatentDataState where

import qualified Data.IntMap.Strict as IntMap
import Algebra.Graph
import UnibenchPatentDataParser

collectAllPatents = do
    categories <- collectCategories "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\processed_dataset\\category.table"
    print $ length categories 
    assignees <- collectAssignees "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_assignee_semicolon.table"
    print $ length assignees
    classes <- collectClasses "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_class_semicolon.table" categories
    print $ length classes
    patents <- collectPatents "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_patent_semicolon.table" assignees classes categories
    print $ IntMap.size patents
    patentGraph <- collectPatentGraph "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_citation.graph" patents
    print $ hasEdge (IntMap.lookup 3858243 patents) (IntMap.lookup 2949611 patents) patentGraph
    inventors <- collectInventors "C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_inventor_piece.table" patents
    print $ length inventors