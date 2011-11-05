--
-- Defines logical connectivity operators
--

module Logic where
    
    -- ** Connectives **

    -- Negation
    -- not Q
    
    -- Conjunction
    -- Q && P
    
    -- Disjunction
    -- Q || P

    -- Implication
    -- Q ==> P
    infix 1 ==>
    (==>) :: Bool -> Bool -> Bool
    False ==> _ = True
    True  ==> x = x
    
    -- Equivalence
    -- Q <=> P
    infix 1 <=>
    (<=>) :: Bool -> Bool -> Bool
    x <=> y = x == y
         
    -- Exlusive Or
    -- Q <+> P
    infix 2 <+>
    (<+>) :: Bool -> Bool -> Bool
    x <+> y = x /= y
    