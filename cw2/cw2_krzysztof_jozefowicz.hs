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
