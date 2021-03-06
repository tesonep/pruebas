import List
import Debug.Trace
import Data.List
import Char
import Data.Maybe

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

triangulo = [[75],[95,64],[17,47,82],[18,35,87,10],[20,04,82,47,65],[19,01,23,75,03,34],[88,02,77,73,07,63,67],[99,65,04,28,06,16,70,92],[41,41,26,56,83,40,80,70,33],[41,48,72,33,47,32,37,16,94,29],[53,71,44,65,25,43,91,52,97,51,14],[70,11,33,28,77,73,17,78,39,68,17,57],[91,71,52,38,17,14,91,43,58,50,27,29,48],[63,66,04,68,89,53,67,30,73,16,69,87,40,31],[04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

adyacentes pos = (pos, pos+1)

replace pos x xs = (take pos xs) ++ x:(drop (pos+1) xs)

sumMax a b c = a + (max b c)

calcLinea arriba abajo = calcLinea' abajo arriba 0

calcLinea' abajo [] pos = []
calcLinea' abajo (x:xs) pos = (calcLinea'' abajo x (adyacentes pos)) : (calcLinea' abajo xs (pos+1))

calcLinea'' abajo x (p1,p2) = sumMax x (abajo!!p1) (abajo!!p2)

calcT t = foldr1 calcLinea t

data DiaSemana = Dom | Lun | Mar | Mie | Jue | Vie | Sab deriving (Show,Enum, Eq)

proximoDiaSemana Sab = Dom
proximoDiaSemana x = succ x

lDiasMes = [31,28,31,30,31,30,31,31,30,31,30,31]

esBiciesto anio = (mod anio 4 == 0  && mod anio 100 /= 0) || (mod anio 400 == 0)

diasMes mes anio
      | mes == 2 && (esBiciesto anio) = 29
      | otherwise = lDiasMes!!(mes-1)	
       
newtype Dia = Dia (Int,Int,Int,DiaSemana) deriving (Show, Eq)

getDiaSemana (Dia (dia,mes,anio,diaSemana)) = diaSemana

getDia (Dia (dia,mes,anio,diaSemana)) = dia

instance Enum Dia where
    succ = proximoDia
    fromEnum (Dia (dia,mes,anio,diaSemana)) = anio*100000 + mes*1000 + dia*10 + (fromEnum diaSemana)
    toEnum val = Dia (anio,mes,dia,diaSemana)
      where anio = div val 100000
            mes  = div (mod val 100000) 1000
            dia  = div (mod val 1000) 10
            diaSemana = toEnum (mod (mod val 10) 7)

proximoDia (Dia (dia,mes,anio,diaSemana)) = Dia (pDia, pMes, pAnio, (proximoDiaSemana diaSemana))
     where 
          ultimoDiaMes = diasMes mes anio
          pDia = if ultimoDiaMes == dia then 1 else dia+1
          pMes = if ultimoDiaMes == dia then (if mes == 12 then 1 else mes+1) else mes
          pAnio = if ultimoDiaMes == dia && mes==12 then anio+1 else anio


desde x = x:(desde (succ x))

hasta (x:xs) y = if x == y then [x] else x:(hasta xs y)

contarDomingosPrimeroMes [] n = n
contarDomingosPrimeroMes (x:xs) n = contarDomingosPrimeroMes xs nn
             where nn = if getDiaSemana x == Dom && getDia x == 1 then n+1 else n 


divisores n = filter (/= n) (map head (group (sort(map product (subsequences (factorizar n))))))

amiguin n = sum (divisores n)

esAmigable n = if a/= n then amiguin (a) == n else False
      where a = amiguin n

problem22 = do 
               nombres <- (readFile "names.txt")
               return (valoresNombre (sort (words nombres)))

valoresNombre xs = valoresNombre' xs 1
       where valoresNombre' (x:xs) n = (calcValorNombre x n) + (valoresNombre' xs (n+1))
             valoresNombre' [] _ = 0

calcValorNombre nombre n = (sumaLetras nombre )* n
       where sumaLetras (x:xs) = (valorLetra x) + (sumaLetras xs)
             sumaLetras []     = 0
             valorLetra l      = (ord l) - (ord 'A') + 1 

esAbundante n = sum (divisores n) > n

abundantes = filter esAbundante [1..]

abundantesHasta n = ah n abundantes
        where ah n (x:xs) = if x <= n then x:(ah n xs) else []

sumaNoAbundantes = sum [1..28000] - sumaParesMayor48 - sum (esSumaAbundantes [1..47]) - sum (map head $ group $ sort armarSumaAbundantes)

esSumaAbundantes xs = sort (g xs)	
        where g [] = []
              g (x:xs) = (f' x (abundantesHasta x)) ++ (esSumaAbundantes xs)
              f' x [] = []
              f' x (y:ys) = if x - y <= 0 then [] else (if esAbundante (x-y) then [x] else (f' x ys))

sumaParesMayor48 = sum [ x | x <- [48..28000], mod x 2 == 0]

armarSumaAbundantes = asa $ abundantesHasta 28000
asa [] = []
asa (x:xs) = asa' x ++ asa xs
asa' x = if mod x 2 == 0 then asa2 x (abundantesHasta 28000) else []
asa2 x [] = []
asa2 x (y:ys) = if x+y >= 28000 then [] else (if mod y 2 == 1 then (x+y):(asa2 x ys) else asa2 x ys)


division n d = div' (mod n d) d 
     where div' 0 d = []
           div' r d = (div (r*10) d):(div' (mod (r*10) d) d)

divisionCiclo n d = let (a,b) = div' (mod n d) d [mod n d] in (d,a,b)
     where div' 0 _ _ = ([],-1)
           div' r d restos = if elem (mod (r*10) d) restos 
		                     then ([div (r*10) d], fromJust	$ elemIndex (mod (r*10) d) restos )
							 else 
							  let (a,b) = (div' (mod (r*10) d) d (restos ++ [(mod (r*10) d)])) in ((div (r*10) d):a,b)
		   
problem26 = map f $ filter (\(d,a,b) -> b /= (-1)) $  map (divisionCiclo 1) [1..1000]
     where f (d,a,b) = (d, length (drop b a))
	 
max' (a,b) (x,y) = if b > y then (a,b) else (x,y)

esPrimo p = p > 1 && (all (\n -> p `mod` n /= 0 ) $ takeWhile (\n -> n*n <= p) [2..])

cantidadDePrimosEnCuadratica a b = f' a b (-1)
     where f' a b n = if esPrimo ( (n+1)^2 + a*(n+1) + b) then f' a b (n+1) else (n+1)
	 
problem27 = foldl1 f' $ filter (\(a,b,c)-> c > 1) [(x,y, cantidadDePrimosEnCuadratica x y) | x <- [-999..999], y<- [0..999], esPrimo (abs y)]
     where f' (a,b,c) (x,y,z) = if c > z then (a,b,c) else (x,y,z)

vuelta n neAnterior = se:sw:nw:ne:[]
       where se = neAnterior + (2*n);
	         sw = se + (2*n);
			 nw = sw + (2*n);
			 ne = nw + (2*n);
			 
p28 = p28' 1 [1]

p28' 501 ds = ds
p28' n ds = p28' (n+1) $ ds ++ (vuelta n (last ds))

p30  =  filter f' p30'
  where f' xs = (sum $ map (^5) xs) == ((read $ map intToDigit xs)::Int)
p30' = [a:b:c:d:e:f:[]| a <- [0..9], b <-[0..9], c <-[0..9], d <-[0..9], e <-[0..9], f <-[0..9]]


p31_coins = [1,2,5,10,20,50,100,200]
p31_posibles 0 = []
p31_posibles n = filter (<=n) p31_coins

