pisz2 :: String -> String
pisz2 x = x ++ "!"

intt :: (RealFrac a, Integral a, Eq a) => a -> Bool
intt a = round a == a
