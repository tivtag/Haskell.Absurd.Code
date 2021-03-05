--
-- A slow but elegant qsort implementation
--
-- Usage: qsort [2, 10, 5, 1, 12, 0, 14]
--

module QSort where

   qsort :: Ord a => [a] -> [a]
   qsort []    = []
   qsort (h:t) = lesser ++ [h] ++ bigger
                 where lesser = qsort (filter (<= h) t)
                       bigger = qsort (filter (> h) t)
