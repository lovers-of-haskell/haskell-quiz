data Item = Num String | Division | Times | Minus | Plus deriving Show

strToItem :: String -> Item
strToItem "/" = Division
strToItem "*" = Times
strToItem "-" = Minus
strToItem "+" = Plus
strToItem x = Num x

strToItemList :: String -> [Item]
strToItemList = reverse . foldl (\acc x -> strToItem x : acc) [] . words

make = snd . head . foldl culc [] . strToItemList
  where
    culc (x:y:xs) Division = (2, tos 2 " / " x y) : xs
    culc (x:y:xs) Times = (2, tos 2 " * " x y) : xs
    culc (x:y:xs) Minus = (1, tos 1 " - " x y) : xs
    culc (x:y:xs) Plus = (1, tos 1 " + " x y) : xs
    culc xs (Num x)= (9, x) : xs

tos n mark s1 s2 = aux n s2 ++ mark ++ aux n s1
aux n (a,s)
  | n > a = "(" ++ s ++ ")"
  | otherwise = s
