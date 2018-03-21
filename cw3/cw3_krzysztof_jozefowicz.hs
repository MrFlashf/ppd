-- 1)
powerlist [] = [[]]
powerlist (x:xs) = (powerlist xs) ++ (map(x:) (powerlist xs))

-- 2)
list_contains list x | elem x list = True
                     | otherwise = False
cz_wspl list1 list2 = filter (\n->(list_contains list2 n)) (list1)

-- 3)
add :: (Foldable t, Eq a) => t a -> [a]
add = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []
suma_zb a b = add(a ++ b)

-- 4)
--a) 6 / (12 / (24 / (8 / 2))) = 6 / (12 / (24 / 4)) = 6 / (12 / 6) = 6 / 2 = 3
--b) 1>2 && (3>2 && (5==5 && True)) = False && (True && True) == False && True == False
--c) max 3(max 6(max 12(max 4(max 55(max 11(max 18))))))
-- ==> max 3(max 6(max 12(max 4(max 55(max(11, 18))))))
-- ==> max 3(max 6(max 12(max 4(max 55, 18))))
-- ==> max 3(max 6(max 12(max 4, 55)))
-- ==> max 3(max 6(max 12, 55))
-- ==> max 3(max(6, 55))
-- ==> max 3, 55
-- ==> 55

--d) max 3(max 6(max 12(max 4(max 55(max 11(max 81))))))
-- ==> max 3(max 6(max 12(max 4(max 55(max(11, 81))))))
-- ==> max 3(max 6(max 12(max 4(max 55, 81))))
-- ==> max 3(max 6(max 12(max 4, 81)))
-- ==> max 3(max 6(max 12, 81))
-- ==> max 3(max(6, 81))
-- ==> max 3, 81
-- ==> 81

--e)
-- ==> (\x y -> (x+y)/2) 24 (foldr (\x y -> (x+y)/2) 54 [4,10,6] )
-- ==> (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 (foldr (\x y -> (x+y)/2) 54 [10,6] ))
-- ==> (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 ((\x y -> (x+y)/2) 10(foldr (\x y -> (x+y)/2) 54 [6] )))
-- ==> (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 ((\x y -> (x+y)/2) 10((\x y -> (x+y)/2) 6 (foldr (\x y -> (x+y)/2) 54 [] )))))
-- ==> (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 ((\x y -> (x+y)/2) 10((\x y -> (x+y)/2) 6 54))))
-- ==> (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 ((\x y -> (x+y)/2) 10 30))
-- ==> (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 20)
-- ==> (\x y -> (x+y)/2) 24 12
-- ==> 18

--f)
-- ==> foldl (\x y -> (x+y)/2) ((\x y -> (x+y)/2) 54 2) [4,10,6] 
-- ==> foldl (\x y -> (x+y)/2) (((\x y -> (x+y)/2) (\x y -> (x+y)/2) 54 2) 4) [10,6] 
-- ==> foldl (\x y -> (x+y)/2) (((\x y -> (x+y)/2)((\x y -> (x+y)/2) (\x y -> (x+y)/2) 54 2) 4) 10) [6] 
-- ==> foldl (\x y -> (x+y)/2) (((\x y -> (x+y)/2)((\x y -> (x+y)/2)((\x y -> (x+y)/2) (\x y -> (x+y)/2) 54 2) 4) 10)6) [] 
-- ==>(((\x y -> (x+y)/2)((\x y -> (x+y)/2)((\x y -> (x+y)/2) (\x y -> (x+y)/2) 54 2) 4) 10)6)  
-- ==>((\x y -> (x+y)/2)((\x y -> (x+y)/2)((\x y -> (x+y)/2) 28 4) 10)6) 
-- ==>((\x y -> (x+y)/2)((\x y -> (x+y)/2) 16 10)6) 
-- ==>((\x y -> (x+y)/2)13 6) 
-- ==> 9.5

--g) (((64/4)/2)/4) = ((16/2)/4) = 8/4 = 2

--h) 
-- ==> foldl (\x y -> 2*x + y) ((\x y -> 2*x + y) 8 1) [2,3]
-- ==> foldl (\x y -> 2*x + y) ((\x y -> 2*x + y)((\x y -> 2*x + y) 8 1) 2) [3]
-- ==> foldl (\x y -> 2*x + y) ((\x y -> 2*x + y)((\x y -> 2*x + y)((\x y -> 2*x + y) 8 1) 2) 3) []
-- ==> ((\x y -> 2*x + y)((\x y -> 2*x + y)((\x y -> 2*x + y) 8 1) 2) 3) 
-- ==> (\x y -> 2*x + y) 36 3) 
-- ==> 75

-- 5)
belongs_foldl elem list = foldl (\acc x -> if x == elem then True else acc) False list

-- 6)
custom_map :: (a->b)->[a] -> [b]
custom_map func list = foldl(\acc x -> acc ++ [func x]) [] list

-- 7)
custom_last :: [a] -> a
custom_last = foldl1 (\_ x -> x)

custom_head :: [a] -> a
custom_head = foldr1 (\x _ -> x)

custom_max :: (Ord a) => [a] -> a
custom_max = foldr1 (\a acc -> if a > acc then a else acc)

-- 9)
add_seven list = map (\x -> x+7) list
-- add_seven [1,2] = [8,9]
get_even list = filter (\x -> (x `mod` 2) == 0) list
-- get_even [1,2,3,4] = [2,4]
func list = map (\x -> [x]) list
-- func [1,2,3] = [[1],[2],[3]]
create_tuples list = map (\x -> ("some string", x)) list
-- create_tuples [1,2] = [("some string",1),("some string",2)]
change_tuples list = map (\(x,y) -> (x*2,y*x)) list
-- change_tuples [(1,2),(3,4)] = [(2,2),(6,12)]
multiply_even list = map (\x -> x*2) (filter (\x -> (x `mod` 2 == 0)) list)
-- multiply_even [1,2,3,4,5] = [4,8]
