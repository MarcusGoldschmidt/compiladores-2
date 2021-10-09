module Utils where

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

last' :: [a] -> Maybe a
last' (x : xs) =
  if null xs
    then Just x
    else last' xs
last' [] = Nothing

tail1 :: [a] -> [a]
tail1 (_ : xs) = xs
tail1 _ = []

partitionByDelimiter :: (a -> Bool) -> [a] -> ([a], [a])
partitionByDelimiter f xs =
  (first, second)
  where
    first = takeWhile (not . f) xs
    second = dropWhile (not . f) xs

findAndReplace :: (a -> Bool) -> a -> [a] -> [a]
findAndReplace f replace list =
  first ++ [replace] ++ tail second
  where
    (first, second) = partitionByDelimiter f list

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs