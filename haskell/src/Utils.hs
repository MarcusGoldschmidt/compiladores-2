module Utils where

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

tail1 :: [a] -> [a]
tail1 (_ : xs) = xs
tail1 _ = []