import Test.QuickCheck

l :: [a] -> Int
l [] = 0
l (_:xs) = 1 + l xs

prop_l :: [String] -> Bool
prop_l xs = l xs == length xs
