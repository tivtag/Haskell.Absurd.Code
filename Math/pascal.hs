module PascalTriangle
where
	pascal :: Int -> [[Int]]
	pascal n =
		take n $ iterate pascalRow [1]

	pascalRow :: [Int] -> [Int]
	pascalRow prevRow = 
		zipWith (+) (0 : prevRow) (prevRow ++ [0])