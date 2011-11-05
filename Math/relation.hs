--
-- Module for working with and discovering of Relations and Relation Properties. 
-- Relations are represented as functions.
--

module Relation where
    import Set
    import Logic
    
    type Rel a = a -> a -> Bool
    type RelProp a = (Set a) -> Rel a -> Bool
            
    -- Composes two relations
    comp :: Set a -> Rel a -> Rel a -> Rel a
    comp (Set xs) r s x y = or [r x z && s z y |z <- xs]

    -- Repeats the given relation n times.
    repeatRel :: Integer -> Set a -> Rel a -> Rel a
    repeatRel 0 _ _ = error "can't repeat 0 times"
    repeatRel 1 _ r = r
    repeatRel n xs r = comp xs r (repeatRel (n-1) xs r)
    
    --
    -- Properties:
    --
   
    -- Combines two relation properties into one
    (<&>) :: RelProp a -> RelProp a -> RelProp a
    f <&> g = \s r -> (f s r) && (g s r)
    
    -- Ax (x R x)
    reflexive :: RelProp a
    reflexive (Set xs) r = and [r x x | x <- xs]
    
    -- Ax !(x R x)
    irreflexive :: RelProp a
    irreflexive xs r = not (reflexive xs r)

    -- Ax,y (x R y -> y R x)
    symmetric :: RelProp a
    symmetric (Set xs) r = and [r x y ==> r y x | x <- xs, y <- xs]
        
    -- Ax,y (x R y -> !(y R x))
    asymmetric :: RelProp a
    asymmetric (Set xs) r = and [r x y ==> not (r y x) | x <- xs, y <- xs]
        
    -- Ax,y (x R y ^ y R x -> x = y)
    antisymmetric :: Eq a => RelProp a
    antisymmetric (Set xs) r = and [r x y && r y x ==> x == y | x <- xs, y <- xs]    
        
    -- Ax,y,z (x R y ^ y R z -> x R z)
    transitive :: RelProp a
    transitive (Set xs) r = and [r x y && r y z ==> r x z | x <- xs, y <- xs, z <- xs]
        
    -- Ax,y,z (x R y ^ y R z -> !(x R z))
    intransitive :: RelProp a
    intransitive (Set xs) r = and [r x y && r y z ==> not (r x z) | x <- xs, y <- xs, z <- xs]

    -- linear (or has the "comparison" property")
    -- Ax, xRy || yRx || x=y
    linear :: Eq a => (Set a) -> (Rel a) -> Bool
    linear (Set xs) r = and [r x y || r y x || x == y | x <- xs, y <- xs]
    
    -- transitive ^ reflexive
    preorder :: RelProp a
    preorder = transitive <&> reflexive
    
    -- transitive ^ irreflexive
    strict_partial_order :: RelProp a
    strict_partial_order = transitive <&> irreflexive
    
    -- transitive ^ reflexive ^ antisymmetric
    partial_order :: Eq a => RelProp a
    partial_order = transitive <&> reflexive <&> antisymmetric
   
    -- partial_order ^ linear
    total_order :: Eq a => RelProp a
    total_order = partial_order <&> linear
        
    -- symmetric ^ reflexive ^ transitive
    equivalence :: RelProp a
    equivalence = symmetric <&> reflexive <&> transitive
    
    -- finds various properties of the given relation acting on the given set
    test_relation :: Eq a => (Set a) -> (Rel a) -> [String]
    test_relation xs r = [name | (name, result) <- rs, result]    
        where rs = [
                    ("reflexive",            reflexive xs r),
                    ("irreflexive",          irreflexive xs r),
                    ("symmetric",            symmetric xs r),
                    ("asymmetric",           asymmetric xs r),
                    ("antisymmetric",        antisymmetric xs r),
                    ("transitive",           transitive xs r), 
                    ("intransitive",         intransitive xs r),
                    ("linear",               linear xs r),
                    ("preorder",             preorder xs r),
                    ("strict partial order", strict_partial_order xs r),
                    ("partial order",        partial_order xs r),
                    ("total order",          total_order xs r),
                    ("equivalence",          equivalence xs r)
                ]
                 
    -- Examples
    -- test_relation (Set ['a'..'z']) (\x y -> x < y)
    -- test_relation (Set [1..10]) (\x y -> (x `mod` y) == 0)
   