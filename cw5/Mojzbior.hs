module Mojzbior (podzbior, iloczyn, suma, roznica) where
  --Zad 1)
  iloczyn list_a list_b = filter (\n->(list_contains list_a n)) (list_b)
  list_contains list x | elem x list = True
                       | otherwise = False

  suma list_a list_b = add (list_a ++ list_b)
  add :: (Foldable t, Eq a) => t a -> [a]
  add = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

  podzbior :: (Eq a) => [a] -> [a] -> Bool
  podzbior [] _ = True
  podzbior list_a list_b = if elem (head list_a) list_b then podzbior (tail list_a) list_b
                                                        else False

  roznica :: (Eq a) => [a] -> [a] -> [a]
  roznica [] _ = []
  roznica list_a list_b = reverse (roznica_helper list_a list_b [])
    where roznica_helper [] _ l3 = l3
          roznica_helper list_a list_b l3 =
            if elem (head list_a) (iloczyn list_a list_b) then roznica_helper (tail list_a) list_b l3
                                                          else roznica_helper (tail list_a) list_b ((head list_a):l3)
