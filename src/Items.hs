module Items where

data Weapon
  = Sword
  | Axe
  deriving (Show)

data Item
  = GoldItem Int
  | WeaponItem Weapon
  deriving (Show)

itemChar :: Item -> Char
itemChar (GoldItem _) = '*'
itemChar (WeaponItem _) = '/'
