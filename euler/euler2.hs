import Data.List
import Debug.Trace

isPandigital n = isPandigital' (show n) '9'
isPandigital' n digits= tiene0 n && tieneOtrosDigitos n digits
tiene0 n = (elemIndices '0' n == [] )
tieneOtrosDigitos n digits= all (\c -> length (elemIndices c n) == 1 ) ['1'..digits]

p32 = sum $ nub $ sort $ map (\(a,b) -> a*b) p32'

p32' = filter checkPanDigital ([ (x,y) | x <- [1..999], y <- [1..9999]] :: [(Int,Int)])

checkPanDigital (a,b) = let str = show a ++ show b ++ show (a*b) in length str == 9 && isPandigital ((read str)::Int)
