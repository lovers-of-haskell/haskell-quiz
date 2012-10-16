import Data.Char
import System.IO
import System.Environment

main = do
  number <- getArgs
  putStrLn . show . validate . head $ number

checkAmex :: [Int] -> Bool
checkAmex all@(x:y:xs) = x == 3 && (y == 4 || y == 7) && length all == 15
checkAmex x = False

checkDiscover :: [Int] -> Bool
checkDiscover all@(x:y:z:a:xs) = x == 3 && y == 0 && z == 1 && a == 1 && length all == 16
checkDiscover x = False

checkMaster :: [Int] -> Bool
checkMaster all@(x:y:xs) = x == 5 && elem y [1..5] && length all == 16
checkMaster x = False

checkVisa :: [Int] -> Bool
checkVisa all@(x:xs) = x == 4 && (length all == 13 || length all == 16)
checkVisa x = False

li :: String -> [Int]
li = foldl (\acc x -> digitToInt x : acc) []

add :: [Int] -> [Int]
add [] = []
add (x:[]) = x : []
add (x:y:xs) = x : y * 2 : add xs

digitSum :: [Int] -> Int
digitSum = foldl (\acc x -> (if x >= 10 then x - 9 else x) + acc) 0

validate :: String -> Bool
validate a = check && (digitSum . add . li $ a) `mod` 10 == 0
  where
    l = reverse . li $ a
    check = checkAmex l || checkDiscover l || checkMaster l || checkVisa l