-- a)
f x | x > 2 = x*x | x > 0 && x <= 2 = x-1 | otherwise = abs x

-- b)
nwd a 0 = a
nwd a b = nwd b (mod a b)

-- c)
nww a b = div (a*b) (nwd a b)

-- d)
check a b c = (a + b > c) && (a + c > b) && (b + c > a)

-- e)
obj r h = (1 / 3) * pi * r * r * h

-- f)
two r h = sqrt(h*h + r*r)

-- g)

pow a n = if (n == 0) then 1
                      else a * pow a (n - 1) 

-- h)
powt a 0 = 1
powt a n = a * pow a (n-1)


-- i)
fib 0 = 0
fib 1 = 1
fib n = n + fib (n - 1)

fib10 n | n == fib 10 = True | otherwise = False

-- j)
is_in_fib n = is_in_fib_helper n 1
is_in_fib_helper n k | n == fib k = True
                     | n > fib k = is_in_fib_helper n (k + 1)
                     | otherwise = False

