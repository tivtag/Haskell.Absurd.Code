--
-- A silly test of the Hspec specification/testing libary.
--

module Set.Specs where
    import Set
    import Test.Hspec
    import Test.Hspec.QuickCheck
    
    specs = describe "power set" [
        it "should generate the right power set for {1, 2, 3}" 
            ((powerset [1, 2, 3]) == [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1,2,3]])
        ]

    main = hspec specs