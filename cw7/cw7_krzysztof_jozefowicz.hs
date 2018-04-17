-- zad 7)
-- a)
sort :: [Int] -> [Int]
sort (x:xs) = sort_h [x] xs

sort_h :: [Int] -> [Int] -> [Int]
sort_h sorted [] = sorted
sort_h sorted (x:xs) = sort_h (insert x sorted) xs


insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y : insert x ys
    
-- insert x [y] | x < y = [x]++[y] | otherwise = [y]++[x]

-- b)
bubble_sort [] = []
bubble_sort list = 
    let t1 = bubble_sort_h list
    in bubble_sort (init t1) ++ [last t1]

bubble_sort_h (x:y:xs) | x > y = y : bubble_sort(x:xs)
                     | otherwise = x : bubble_sort(y:xs)
bubble_sort_h (x) = (x)

-- zad 10)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- a)
shortest Empty = 0
shortest (Node a l r) = 1 + min (shortest l) (shortest r)

-- b)
longest Empty = 0
longest (Node a l r) = 1 + max (longest l) (longest r)

-- zad 14)
sort_pairs :: (Ord a, Floating a) => [(a, a)] -> [(a, a)]
sort_pairs (x:xs) = sort_pairs_h [x] xs

sort_pairs_h :: (Ord a, Floating a) => [(a, a)] -> [(a, a)] -> [(a, a)]
sort_pairs_h sorted [] = sorted
sort_pairs_h sorted (x:xs) = sort_pairs_h (insert_pairs x sorted) xs

insert_pairs :: (Ord a, Floating a) => (a, a) -> [(a, a)] -> [(a, a)]
insert_pairs x [] = [x]
insert_pairs x (y:ys) | lx < ly = x:y:ys
                      | otherwise = y : insert_pairs x ys
    where lx = get_dist x
          ly = get_dist y

get_dist :: (Ord a, Floating a) => (a,a) -> a
get_dist (a,b) = sqrt ((a-0)**2 + (b-0)**2)

-- zad 15)
primes = filterPrime [2..]
  where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

-- a)
pierwsze = zip indexes primes
  where indexes = [1..]


-- b)
p n = primes !! (n-1)

