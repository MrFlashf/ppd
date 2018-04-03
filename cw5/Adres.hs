class Adres a where
  -- has_at_sign zwraca True jeśli adres zawiera znak `@`
  has_at_sign :: a -> Bool

data Email = Email [Char] deriving (Show)


instance Adres Email where
  has_at_sign (Email email) = elem '@' email
