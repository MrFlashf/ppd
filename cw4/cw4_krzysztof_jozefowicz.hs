-- zad 1)
data Moto = Seat | BMW | Ford | Jaguar | AlfaRomeo
  deriving (Show)
type Kraj = [Char]
-- a)
get_brand :: Kraj -> Moto
get_brand kraj = case kraj of
  "Germany" -> BMW
  "Spain" -> Seat
  "America" -> Ford
  "Great Britain" -> Jaguar
  "Italia" -> AlfaRomeo
-- b)
get_speed :: Moto -> Double
get_speed car = case car of
  BMW -> 240
  Seat -> 190
  Ford -> 200
  Jaguar -> 250
  AlfaRomeo -> 250

-- zad 2)
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)
-- a)
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
check_order tree =
  (preorder tree == [1,2,4,5,8,3,6,9,7])
  && (inorder tree == [4,2,5,8,1,6,9,3,7])
  && (postorder tree == [4,8,5,2,9,6,7,3,1])

-- b)
tree_b :: Tree Char
tree_b =
  Node 'a'
    (Node 'b' Empty
      (Node 'd' (Node 'f' Empty Empty) Empty)
    )
    (Node 'c'
      (Node 'e' Empty (Node 'g' Empty Empty))
      Empty
    )

check_order_b tree =
  (preorder tree == "abdfceg")
  && (inorder tree == "bfdaegc")
  && (postorder tree == "fdbgeca")

-- zad 3)
tree_member_pre :: (Eq a) => a -> Tree a -> Bool
tree_member_pre _ Empty = False
tree_member_pre el (Node a l r)
  | a == el = True
  | otherwise =
    tree_member_pre el l || tree_member_pre el r

tree_member_in :: (Eq a) => a -> Tree a -> Bool
tree_member_in _ Empty = False
tree_member_in el (Node a l r) = tree_member_in el l || a == el || tree_member_in el r

tree_member_post :: (Eq a) => a -> Tree a -> Bool
tree_member_post _ Empty = False
tree_member_post el (Node a l r) =
  tree_member_post el l
  || tree_member_post el r
  || a == el

-- Zad 4)
-- poddrzewa do sprawdzenia
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

-- tę funkcję wywołuje użytkownik:
subtree_start tree1 tree2
  | subtree tree1 tree2 == True = True
  | subtree tree2 tree1 == True = True
  | otherwise = False

-- funkcje pomocnicze
subtree Empty _ = False
subtree (Node a1 l1 r1) (Node a2 l2 r2)
  | are_same (Node a1 l1 r1) (Node a2 l2 r2) == True = True
  | otherwise =
    subtree l1 (Node a2 l2 r2) || subtree r1 (Node a2 l2 r2)

are_same Empty Empty = True
are_same Empty _ = False
are_same _ Empty = False
are_same (Node a1 l1 r1) (Node a2 l2 r2) =
  a1 == a2 && are_same l1 l2 && are_same r1 r2

-- Zad 5)
bfs :: Tree a -> [a]
bfs Empty = []
bfs tree = bfshelp [tree]
  where
    bfshelp [] = []
    bfshelp xs = map node xs ++ bfshelp (concat (map leftnright xs))

    node (Node a _ _) = a

    leftnright (Node _ Empty Empty) = []
    leftnright (Node _ Empty b) = [b]
    leftnright (Node _ a Empty) = [a]
    leftnright (Node _ a b) = [a,b]
