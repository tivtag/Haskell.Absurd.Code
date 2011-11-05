--
-- Provides various utility functions for boolean logic.
-- Includes:
--     Generation of truth tables, testing function equivalence, tautology.
--     Proofs for various law of logic.

module Proof where
    import Logic
                
    -- ** Constants **
    bools = [True, False]
   
   
    --  ** Truth Tables **
    
    truths1 :: (Bool -> Bool) -> [Bool]
    truths1 f = [f p | p <- bools]
    
    truths2 :: (Bool -> Bool -> Bool) -> [Bool]
    truths2 f = [f p q | p <- bools, q <- bools]
   
    truths3 :: (Bool -> Bool -> Bool -> Bool) -> [Bool]
    truths3 f = [f p q r | p <- bools, q <- bools, r <- bools]
   
    truths4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> [Bool]
    truths4 f = [f p q r s | p <- bools, q <- bools, r <- bools, s <- bools]
   
    -- Tests whether the truth tables of two 1-bool functions match
    -- eq1 induce_self not
    eq1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
    eq1 f f' = (truths1 f) == (truths1 f')
   
    -- Tests whether the truth tables of two 2-bool functions match
    eq2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
    eq2 f f' = (truths2 f) == (truths2 f')
      
    -- Tests whether the truth tables of two 3-bool functions match
    eq3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
    eq3 f f' = (truths3 f) == (truths3 f')
       
    -- Tests whether the truth tables of two 4-bool functions match
    eq4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
    eq4 f f' = (truths4 f) == (truths4 f')
    
   
    -- ** Tautology Tests **
   
    valid1 :: (Bool -> Bool) -> Bool
    valid1 f = (f True) && (f False)
    
    valid2 :: (Bool -> Bool -> Bool) -> Bool
    valid2 f = and (truths2 f)
               
    valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
    valid3 f = and (truths3 f)
          
    valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
    valid4 f = and (truths4 f) 
     
     
    -- ** Sample Formulas **    
    exluded_middle :: Bool -> Bool
    exluded_middle p = p || not p
    
    induce_self :: Bool -> Bool
    induce_self p = p ==> p
    
    induce_two :: Bool -> Bool -> Bool
    induce_two p q = p ==> (q ==> p)
       
    some_formula :: Bool -> Bool -> Bool
    some_formula p q = ((not p) && (p ==> q) <=> not (q && (not p)))
    
    
    -- ** De Morgan's Law    
    -- valid2 (\a -> (\b -> not(a && b) <=> (not a || not b)))
    
    
    -- verifies various theorems
    -- a, b and c can be any boolean formula
    
    verify_theorems :: Bool
    verify_theorems =
        and [
                -- law of double 
                eq1 id (not . not), -- a = !(!a)
                
                -- laws of idempotence
                eq1 id (\a -> a && a),  -- a = a ^ a
                eq1 id (\a -> a || a),  -- a = a || a
                
                -- laws of contraposition
                eq2 (\a b -> a ==> b)         (\a b -> not a || b),     --   a => b  = !a || b
                eq2 (\a b -> not (a ==> b))   (\a b -> a && (not b)),   -- !(a => b) = a ^ !b
                eq2 (\a b -> not a ==> not b) (\a b -> b ==> a),        --  !a => !b = b => a
                eq2 (\a b -> a ==> not b)     (\a b -> b ==> not a),    --   a => !b = b => !a
                eq2 (\a b -> not a ==> b)     (\a b -> not b ==> a),    --  !a => b  = !b => a
                
                eq2 (\a b -> a <=> b) (\a b -> (a ==> b) && (b ==> a)),       -- a <=> b = (a => b) ^ (b => a)                
                eq2 (\a b -> a <=> b) (\a b -> (a && b) || (not a && not b)), -- a <=> b = (a && b) || (!a && !b)
              
                -- laws of commutativity
                eq2 (\a b -> a && b) (\a b -> b && a), -- a ^ b = b ^ a
                eq2 (\a b -> a || b) (\a b -> b || a), -- a || b = b || a
                
                -- De Morgan's Law                
                eq2 (\a b -> not (a && b)) (\a b -> not a || not b), -- !(a ^ b) = !a || !b
                eq2 (\a b -> not (a || b)) (\a b -> not a && not b), -- !(a || b) = !a ^ !b
                
                -- laws of associavity
                eq3 (\a b c -> a && (b && c)) (\a b c -> (a && b) && c), -- a ^ (b ^ c) == (a ^ b) ^ c
                eq3 (\a b c -> a || (b || c)) (\a b c -> (a || b) || c), -- a || (b || c) == (a || b) || c
                
                -- distribution laws
                eq3 (\a b c -> a && (b || c)) (\a b c -> (a && b) || (a && c)), -- a ^ (b || c) == (a ^ b) || (a ^ c)
                eq3 (\a b c -> a || (b && c)) (\a b c -> (a || b) && (a || c))  -- a || (b ^ c) == (a || b) ^ (a || c)
            ]
    
    verify_principles :: Bool
    verify_principles = 
        and [
                not True == False, -- !T == F
                not False == True, -- !F == T
                
                eq1 (\b -> b ==> False) (\b -> not b), -- b => False = !b
                
                -- dominance laws
                eq1 (\b -> b || True) (\_ -> True),    -- b || True = True 
                eq1 (\b -> b && False) (\_ -> False),  -- b ^ False = False  
                
                -- identity laws
                eq1 (\b -> b || False) (\b -> b),      -- b || False = b   
                eq1 (\b -> b && True) (\b -> b),       -- b ^ True = b

                -- law of excluded middle
                eq1 (\b -> b || not b) (\_ -> True),   -- b || !b = True
                
                -- contradiction
                eq1 (\b -> b && not b) (\_ -> False)   -- b && b = False                
            ]
        
    -- | Counts the number of times that the given predicate holds.
    count :: (a -> Bool) -> [a] -> Int
    count p = length . filter p
        
    -- | Tests whether the given property occurs once, and only once.
    unique :: (a -> Bool) -> [a] -> Bool
    unique p xs = 1 == count p xs
    
    -- | Tests whether the input list got an even number of True values.
    parity :: [Bool] -> Bool
    parity = even . count id
    
    -- | Tests whether the given property holds an even 
    --   number of times for the given list.
    evenNR :: (a -> Bool) -> [a] -> Bool
    evenNR p = parity . map p
    




   