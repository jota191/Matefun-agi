module Language.MFAG.Utils where

wrapParen s = "(" ++ s ++ ")"
wrapSpace s = " " ++ s ++ " "

append3 x y z = x ++ y ++ z
append5 x y z w t = x ++ y ++ z ++ w ++ t
append6 a x y z w t = a ++ x ++ y ++ z ++ w ++ t

-- liftA5 :: Applicative f =>
--   (a -> b -> c -> d -> e -> g)
--   -> f a -> f b -> f c -> f d -> f e -> f g
-- liftA5 f fa fb fc fr fg =
--   f <$> 
