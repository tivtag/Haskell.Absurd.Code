-- Permutation Groups
import Data.List

uniset [] = []
uniset (x:xs) = sort [a|b <- (x:xs), a <- b]

perm 1 = [[1]]
perm 2 = [[1,2],[2,1]]
perm n = uniset[
            [insort n x|n >= 2,m <- [1..(n - 1)],x <- perm (n - 1),
            let insort n x = take (n - (m + 1)) x  ++ [n] ++ drop (n - (m + 1)) x],

            [insort n x|n >= 2,m <- [1..(n - 1)],x <- perm (n - 1),
            let insort n x = take (n - m) x  ++ [n] ++ drop (n - m) x]
        ]


-- Compile by invoking "ghc --make -O perm.hs" on the command line                
main :: IO()    
main = do
    putStrLn "\n** Permutationgroups **\r\n"
    putStrLn "Please enter N"
    input <- getLine
    
    let n = read input :: Int
    let p = perm n
    putStrLn (show p)
    
    getLine
    return ()
