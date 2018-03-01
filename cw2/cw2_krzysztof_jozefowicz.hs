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
