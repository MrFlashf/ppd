-- 1)
 -- a)
add_head list el = el : list

 -- b)
add_second list el = head list : el : tail list

 -- c)
add_tail list el = list ++ [el]

-- 2)
 -- a)
second_element list = list !! 1

 -- b)
third_element list = list !! 2

 -- c)
sec_to_last list = list !! (length list - 2)

-- 3)
custom_reverse [] mynew = mynew
custom_reverse myhead mynew = custom_reverse (tail myhead) ([head myhead] ++ mynew)
custom_reverse_start list = custom_reverse list []

-- 4)
last_element list = list !! (length list - 1)

edge_reverse list = (last_element list) : (tail (take (length list - 1) list)) ++ [head list]

-- 5)
 -- a)
sum_even list = length (filter (even) list)

 -- b)
by_three_start n = by_three n 0
by_three 0 score = score
by_three n score | (mod n 3) == 0 = by_three (n-1) (score + 1) | otherwise = by_three (n-1) (score)

 -- c)
by_three_sum_start n = by_three_sum n 0
by_three_sum 0 score = score
by_three_sum n score | (mod n 3) == 0 = by_three_sum (n-1) (score + n) | otherwise = by_three_sum (n-1) (score)

-- 6)
is_length_even list | mod (length list) 2 == 0 = True | otherwise = False

-- 7)
 -- a)
power x = x*x
power_list list = map power list

 -- b)
power_no_map_start list = power_no_map list []
power_no_map [] new_list = new_list
power_no_map list new_list = power_no_map (tail list) (new_list ++ [power (head list)])

-- 8)
count char list = length (filter(==char) list)

-- 9) 
rep item n = replicate n item

-- 10)
-- funkcja korzysta z funkcji z zadania 3
palindrom list | custom_reverse_start list == list = True | otherwise = False

-- 11)
delete_first _ [] = []
delete_first el (b:bc) | el == b = bc | otherwise = b : delete_first el bc

-- 12)
delete_n_element :: Int -> [a] -> [a]
delete_n_element _ [] = []
delete_n_element n (a:as) | n == 0 = as | otherwise = a : delete_n_element (n-1) as

-- 13)
contains a list | elem a list = False | otherwise = True
check_list list_a list_b | length(filter (\el->(contains el list_b)) list_a)==0 = True | otherwise = False

-- 14)
change_tuples list = map (\(el1, el2)->(el2, el1)) list
