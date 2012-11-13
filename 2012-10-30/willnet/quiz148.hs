data Item = Num String | Division | Times | Minus | Plus deriving Show
data Type = MP | DT | NB deriving (Eq, Ord)

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
    culc (x:y:xs) Division = (DT, dt y ++ " / " ++ dt x) : xs
    culc (x:y:xs) Times = (DT, dt y ++ " * " ++ dt x) : xs
    culc (x:y:xs) Minus = (MP, mp y ++ " - " ++ mp x) : xs
    culc (x:y:xs) Plus = (MP, mp y ++ " + " ++ mp x) : xs
    culc xs (Num x)= (NB, x) : xs

dt (tp, s)
  | DT > tp = "(" ++ s ++ ")"
  | otherwise = s

mp (tp, s)
  | MP > tp = "(" ++ s ++ ")"
  | otherwise = s

