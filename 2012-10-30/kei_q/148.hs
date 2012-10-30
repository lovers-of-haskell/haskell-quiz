import Numeric
import Data.Monoid ((<>))
import Text.Printf (printf)

data Token = N Double | BinOp String deriving (Eq)
data Expr = Leaf Token | Node Expr Token Expr deriving (Eq)

instance Show Token where
    show (N n) = show n
    show (BinOp op) = op

instance Show Expr where
    show (Leaf n) = show n
    show (Node el t er) = printf "(%s %s %s)" (show el) (show t) (show er)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve = show . parse . tokenizer

tokenizer :: String -> [Token]
tokenizer = map toToken . words

toToken :: String -> Token
toToken t = case readFloat t of
    [] -> BinOp t
    [(n,_)] -> N n

parse :: [Token] -> Expr
parse = head . foldl aux [] where
    aux expr t@(N n) = Leaf t : expr
    aux (a:b:expr) t = Node b t a : expr
