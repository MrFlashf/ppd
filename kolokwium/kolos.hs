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
