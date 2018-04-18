import Data.Char

-- 1 - nty wyraz ciagu
-- rek
an_rek 0 = 0
an_rek 1 = 0
an_rek 2 = 5

an_rek n = (an_rek (n-1)) + 2*(an_rek (n-2))

-- acc
an_acc 1 acc = acc
an_acc 2 acc = 5 + acc
an_acc n acc = acc + (an_acc (n-1) acc) + 2*(an_acc (n-2) acc) 

-- 3 
-- a - pierwszy z drugim
switch (x:y:xs) = y:x:xs

-- b - pierwszy z ost
b_switch (x:xs) = (last1 : no_last) ++ [x]
  where last1 = last xs
        no_last = init xs

-- c - drugi z przed ost
c_switch (x:y:xs) = x : przed_ost : xs_no_last_two ++ [y] ++ [(last xs)]
  where przed_ost = xs !! ((length xs) - 2) 
        xs_no_last_two = init (init xs)

-- 4
d_length d list = length (filter (==d) list)

-- 5
list_equal [] [] = True
list_equal (x:xs) (y:ys) | x == x = list_equal xs ys
                         | otherwise = False
list_equal _ _ = False

-- 8
c_zip [] [] = []
c_zip (x:xs) (y:ys) | x <= y = x : (c_zip xs (y:ys))
                    | x > y = y : (c_zip (x:xs) ys)
c_zip [] q = q
c_zip q [] = q

pp x = (x == [])

-- 9
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

tree :: Tree Int
tree = Node 1
  (Node 2
    (Node 4 Empty Empty)
    (Node 5 Empty
      (Node 8 Empty Empty)
    )
  )
  (Node 3
    (Node 6 Empty
      (Node 9 Empty Empty)
    )
    (Node 7 Empty Empty)
  )



subtree_ok :: Tree Int
subtree_ok = Node 3
    (Node 6 Empty
      (Node 9 Empty Empty)
    )
    (Node 7 Empty Empty)

subtree_false :: Tree Int
subtree_false =
  Node 3
    (Node 6 Empty
      (Node 9 Empty Empty)
    )
    (Node 8 Empty Empty)


is_tree :: Eq a => Tree a -> Tree a -> Bool
is_tree Empty _ = False
is_tree (Node a1 l1 r1) (Node a2 l2 r2) | are_same (Node a1 l1 r1) (Node a2 l2 r2) = True
  | otherwise = is_tree l1 (Node a2 l2 r2) || is_tree r1 (Node a2 l2 r2)

are_same :: Eq a => Tree a -> Tree a -> Bool
are_same Empty Empty = True
are_same (Node a1 l1 r1) (Node a2 l2 r2) = a1 == a2 && are_same l1 l2 && are_same r1 r2
are_same _ _ = False

-- 10
longest :: Eq a => Tree a -> Int
longest (Node a l r) = max (longest_h l 1) (longest_h r 1)

longest_h :: Eq a => Tree a -> Int -> Int
longest_h Empty n = n
longest_h (Node a l r) n = max (longest_h l n+1) (longest_h r n+1) 

-- 13
pairs :: [Int] -> [(Int, Int)]
pairs list = pairs_h list []

pairs_h :: [Int] -> [(Int,Int)] -> [(Int, Int)]
pairs_h [] new_list = new_list
pairs_h (x:xs) new_list = pairs_h filtered new_list1
  where filtered = filter (/=x) (x:xs)
        new_list1 = new_list ++ [(x, (length (filter (==x) (x:xs))))]




