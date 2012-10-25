module L01.Optional where

--  class Optional<a> {
--    Optional(a a) {} // Full
--    Optional() {} // Empty
--  }
data Optional a = Full a | Empty deriving (Eq, Show)

-- data Type <0..n typevars> =
  -- Constructor1 <0..C1args> |
  -- Constructor2 <0..C2args> |
  -- ...
  -- deriving (optional)

-- (a -> b) -> (F a -> F b)
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty    = Empty
mapOptional f (Full a) = Full (f a)

-- (a -> F b) -> (F a -> F b)
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty    = Empty
bindOptional f (Full a) = f a

(??) :: Optional a -> a -> a
Empty ?? d  = d
Full a ?? _ = a

(<+>) :: Optional a -> Optional a -> Optional a
Empty <+> o = o
k <+> _     = k

