data Item = Num String | Division | Times | Minus | Plus deriving Show

strToItem :: String -> Item
strToItem "/" = Division
strToItem "*" = Times
strToItem "-" = Minus
strToItem "+" = Plus
strToItem x = Num x

strToItemList :: String -> [Item]
strToItemList = reverse . foldl (\acc x -> strToItem x : acc) [] . words

culc :: [String] -> Item -> [String]
culc (x:y:xs) Division = (y ++ " / " ++ x) : xs
culc (x:y:xs) Times = (y ++ " * " ++ x) : xs
culc (x:y:xs) Minus = ("(" ++ y ++ " - " ++ x ++ ")") : xs
culc (x:y:xs) Plus = ("(" ++ y ++ " + " ++ x ++ ")") : xs
culc xs (Num x)= x : xs

make :: String -> String
make = head . foldl culc [] . strToItemList
