-- 1)
add_head list el = el : list

add_second list el = head list : el : tail list

add_tail list el = list ++ [el]
