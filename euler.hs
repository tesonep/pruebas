import List
import Debug.Trace
import Data.List

primes = 2: 3: sieve (tail primes) 3 []
   where
    notDivsBy d n     = n `mod` d /= 0
    sieve (p:ps) x ds = foldr (filter . notDivsBy) 
                                [x+2,x+4..p*p-2] ds
                        ++ sieve ps (p*p) (p:ds)


primesTo n = primesTo' n primes
   where
     primesTo' n (x:xs) = if x < n then x:(primesTo' n xs) else []


rawMatrix = [08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08,49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00,81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65,52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91,22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80,24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50,32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70,67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21,24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72,21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95,78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92,16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57,86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58,19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40,04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66,88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69,04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36,20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16,20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54,01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]


strips n xs = if length (take n xs) == 0 then [] else (take n xs):(strips n (drop n xs))

horizontalMatrix = strips 20 rawMatrix

makeGroups matrix = concat (f1 matrix)
         where 
              f1 [] = [[]]
	      f1 (x:xs) = (f2 x):(f1 xs)
              f2 xs = if length (take 4 xs) == 4 then (take 4 xs):(f2 (drop 4 xs)) else (take 4 xs):[]


maxHorizontal = foldl1 max (map product (makeGroups horizontalMatrix))
maxVertical = foldl1 max (map product (makeGroups (transpose horizontalMatrix)))


cutMatrix (x:[]) = [] 
cutMatrix (x:xs) = cutMatrix' xs
         where 
               cutMatrix'(x:[]) = tail x:[]
	       cutMatrix'(x:xs) = tail x:cutMatrix' xs


cutColumn [] = []
cutColumn (x:xs) = tail x : cutColumn xs

(#) matrix (x,y) = (matrix !! y) !! x
                        
diag4 matrix (x,y) = if checkOk matrix x y 
                     then diag4' matrix (x,y) 0
                     else []   

checkOk matrix x y= length (head matrix) > x + 3 && length matrix > y + 3

diag4' matrix (x,y) 4 = []
diag4' matrix (x,y) d = (matrix # ((x+d),(y+d))) : ( diag4' matrix (x,y) (d+1))


diagL4 matrix (x,y) = if checkLOk matrix x y 
                     then diagL4' matrix (x,y) 0
                     else []   

checkLOk matrix x y= length (head matrix) > x + 3 && 0 < y - 4

diagL4' matrix (x,y) 4 = []
diagL4' matrix (x,y) d = (matrix # ((x+d),(y-d))) : ( diagL4' matrix (x,y) (d+1))


maxDiagonal = foldl1 max (map product (filter (\x -> x /= []) [ diag4 horizontalMatrix (x,y) | x <- [0..19], y <- [0..19] ]))
maxDiagonal2 = foldl1 max (map product (filter (\x -> x /= []) [ diagL4 horizontalMatrix (x,y) | x <- [0..19], y <- reverse [0..19] ]))


maxs = max (max maxVertical maxHorizontal) (max maxDiagonal2 maxDiagonal)


triangles = triangles' 1 0
   where triangles' n a = (a+n) : (triangles' (n+1) (a+n))

factorizar n = (factorizar' n primes)

factorizar' 1 _  = []
factorizar' n ps = if resto == 0 then divisor:(factorizar' r primes) else (factorizar' n (tail ps))
         where divisor = head ps 
               r = div n divisor 
               resto = mod n divisor

factores n = (n, length (group (sort(subsequences (factorizar n)))))

serie 1 = [1]
serie n = if even n then n:(serie (div n 2)) else n:(serie (3*n + 1))

paths size = paths' size 1 1

paths' size x y = (right size x y) + (down size x y)

right size x y = if x < size  then (paths' size (x+1) y) else 1
down  size x y = if y < size  then (paths' size x (y+1)) else 1

value _ 0 = 1
value r c = (value r (c-1)) * (((r+1) - c) / c)::Rational

letras 0 = ""
letras 1 = "one"
letras 2 = "two"
letras 3 = "three"
letras 4 = "four"
letras 5 = "five"
letras 6 = "six"
letras 7 = "seven"
letras 8 = "eight"
letras 9 = "nine"
letras 10 = "ten"
letras 11 = "eleven"
letras 12 = "twelve"
letras 13 = "thirteen"
letras 14 = "fourteen"
letras 15 = "fifteen"
letras 16 = "sixteen"
letras 17 = "seventeen"
letras 18 = "eighteen"
letras 19 = "nineteen"

letras 20 = "twenty"
letras 30 = "thirty"
letras 40 = "forty"
letras 50 = "fifty"
letras 60 = "sixty"
letras 70 = "seventy"
letras 80 = "eighty"
letras 90 = "ninety"

letras   x  = (if x > 999 then letrasMiles x else "") ++ (if (mod x 1000) > 99 then letrasCientos (mod x 1000) else "") ++ (letrasDieces (mod x 100))

letrasMiles   x  =  letras (div x 1000) ++ "thousand" ++ (if (mod x 1000) /= 0 then "and" else "")
letrasCientos x  =  letras (div x 100) ++ "hundred" ++ (if (mod x 100) /= 0 then "and" else "")
letrasDieces  x  =  if x <= 19 then letras x else letras ((div x 10)*10) ++ letras (mod x 10)