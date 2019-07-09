module GraphParser where

populateGraphWithData :: [(a, a)] -> (a -> b) -> [(b,b)]
populateGraphWithData [] _ = []
populateGraphWithData ((x,y):xs) f = (f x, f y): populateGraphWithData xs f