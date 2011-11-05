--
-- Silly exerpiment in creating an own Set type.
--

module Set where    
    import Data.List(delete, intersperse)
    
    newtype Set a = Set [a]
    
    -- eq
    instance (Eq a) => Eq (Set a) where
        a == b = (a `subset` b) && (b `subset` a)
    
    -- show
    instance (Show a) => Show (Set a) where
        showsPrec _ (Set s) str = showSet s str
        
    showSet xs = showString ("{" ++ b ++ "}")
                 where b = concat (intersperse "," [show x|x <- xs])
                 
    -- generators
    singleton :: a -> Set a
    singleton x = Set [x]
                 
    -- subset
    subset :: Eq a => Set a -> Set a -> Bool
    subset (Set xs) (Set ys) = and [x `elem` ys | x <- xs]
        
    -- inset
    inset :: Eq a => a -> Set a -> Bool
    inset x (Set ys) = x `elem` ys
    
    -- isEmpty
    isEmpty :: Set a -> Bool
    isEmpty (Set []) = True
    isEmpty _        = False
    
    -- insertSet
    insertSet :: Eq a => a -> Set a -> Set a
    insertSet x (Set ys) | inset x (Set ys) = Set ys
                         | otherwise = Set (x:ys)
    
    -- deleteSet
    deleteSet :: Eq a => a -> Set a -> Set a
    deleteSet x (Set ys) = Set (delete x ys)
        
    -- constants
    emptySet = Set []
        
    powerset [] = [[]]
    powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)