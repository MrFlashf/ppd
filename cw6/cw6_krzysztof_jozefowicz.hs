-- zad 1)

il_dif = do 
    putStrLn "Podaj liczbę a: "
    a <- readLn :: IO Int
    putStrLn "Podaj liczbę b: "
    b <- readLn :: IO Int
    putStrLn ("Suma: " ++ show (a+b))
    putStrLn ("Iloczyn: " ++ show (a*b))
    putStrLn ("Rónica: " ++ show (a-b))

-- zad 2)

nwd a 0 = a
nwd a b = nwd b (mod a b)

nww a b = div (a*b) (nwd a b)

zad2 = do
    putStrLn "Podaj liczbę a: "
    a <- readLn :: IO Int
    putStrLn "Podaj liczbę b: "
    b <- readLn :: IO Int 
    putStrLn ("NWD: " ++ show (nwd a b))
    putStrLn ("NWW: " ++ show (nww a b))

-- zad 3)
zad3 = do
    putStrLn "Podaj imię "
    name <- getLine
    putStrLn "Podaj nazwisko"
    surname <- getLine
    putStrLn ([(head name)] ++ ". " ++ [(head surname)] ++ ".")

-- zad 4)
game = game_h 0

game_h 10 = do
    putStrLn "Koniec Gry!"

game_h counter = do
    putStrLn "Podaj liczbę: "
    n <- readLn :: IO Int
    if n == my_number then
        putStrLn ("Brawo, zgadłeś!")
    else if n > my_number then do
        putStrLn "Liczba jest mniejsza"
        game_h (counter+1)
    else do
        putStrLn "Liczba jest większa!"
        game_h (counter+1)
    where my_number = 45

    