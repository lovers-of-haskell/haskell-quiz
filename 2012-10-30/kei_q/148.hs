import Numeric
import Data.Monoid ((<>))
import Text.Printf (printf)

data Token = N Double | BinOp String deriving (Eq)
data Expr = Leaf Double | Node Expr Token Expr deriving (Show, Eq)

instance Show Token where
    show (N n) = show n
    show (BinOp op) = op

toString :: Token -> Expr -> String
toString _ (Leaf n) = show n
toString p (Node el t er) 
  | check p t = printf "(%s %s %s)" (toString t el) (show t) (toString t er)
  | otherwise = printf "%s %s %s" (toString t el) (show t) (toString t er)

check (BinOp "*") (BinOp "+") = True
check (BinOp "*") (BinOp "-") = True
check (BinOp "/") (BinOp "+") = True
check (BinOp "/") (BinOp "-") = True
check _ _ = False

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve = toString (BinOp "dummy") . parse . tokenizer

tokenizer :: String -> [Token]
tokenizer = map toToken . words

toToken :: String -> Token
toToken t = case readFloat t of
    [] -> BinOp t
    [(n,_)] -> N n

parse :: [Token] -> Expr
parse = head . foldl aux [] where
    aux expr (N n) = Leaf n : expr
    aux (a:b:expr) t = Node b t a : expr

-- (1 * 2 + 3) * 4  #  1 2 * 3 + 4 *
-- (1 + 2 * 3) * 4  #  1 2 3 * + 4 *