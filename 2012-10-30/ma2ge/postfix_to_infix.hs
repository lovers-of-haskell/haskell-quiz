-- solve the following
-- http://rubyquiz.com/quiz148.html

toInfix :: String -> String
toInfix = head . foldl solve [] . words
  where solve (x:y:ys) "*" = concat ["(", y, " * ", x, ")"]:ys
        solve (x:y:ys) "/" = concat ["(", y, " / ", x, ")"]:ys
        solve (x:y:ys) "+" = concat ["(", y, " + ", x, ")"]:ys
        solve (x:y:ys) "-" = concat ["(", y, " - ", x, ")"]:ys
        solve xs number = number:xs


