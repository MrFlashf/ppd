import Data.List
import Mojzbior

-- zad 1)

list_a = [1,2,3]
list_b = [4,5,6]
t = suma list_a list_b

-- zad 2)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

tree_ok = Node 10
  (Node 2
    (Node 4 Empty Empty)
    (Node 8 Empty Empty))
  (Node 15
    (Node 12 Empty Empty)
    (Node 18 Empty Empty))

tree_fail = Node 10
  (Node 15
    (Node 12 Empty Empty)
    (Node 18 Empty Empty))
  (Node 2
    (Node 4 Empty Empty)
    (Node 8 Empty Empty))


are_lower value (Node a l r) | value < a = False
                             | otherwise = (are_lower value l) && (are_lower value r)
are_lower value Empty = True


are_higher value (Node a l r) | value > a = False
                              | otherwise = (are_higher value l) && (are_higher value r)
are_higher value Empty = True

zad2 Empty = False
zad2 (Node a l r) = (are_lower a l) && (are_higher a r)

-- zad 3)
tree = Node 10
  (Node 2
    (Node 5
      (Node 6 Empty Empty)
      (Node 8 (Node 8 Empty Empty) (Node 10 Empty Empty)))
    (Node 3 Empty Empty))
  (Node 5 Empty Empty)

-- a)
shortest Empty = 0
shortest (Node a l r) = 1 + min (shortest l) (shortest r)

-- b)
longest Empty = 0
longest (Node a l r) = 1 + max (longest l) (longest r)

-- zad 5)
pair :: (Eq a) => [a] -> [(a, Int)]
pair [] = []
pair list = pair_helper list []

pair_helper :: (Eq a) => [a] -> [(a, Int)] -> [(a, Int)]
pair_helper [] result = result
pair_helper source result = pair_helper filtered_source (result ++ [(next_elem, quantity)]) where
  next_elem = head source
  filtered_source = filter (\x -> x /= head source) source
  quantity = counter (head source) source

counter :: (Eq a) => a -> [a] -> Int
counter element list = length (filter (\x -> x == element) list)
