import List

expand xs = sort $ expand' xs xs
expand' xs acc = if nl == [] then acc else (expand' nl (acc ++ nl))
	    	where nl = compress $ nextLevel xs

nextLevel :: [[Integer]] -> [[Integer]]
nextLevel [] = []
nextLevel (x:xs) = nextLevel' [] x ++ nextLevel xs

reemplazos = [
		(2, [1,1]),
		(5, [2,2,1]),
		(10, [5,5]),
		(20, [10,10]),
		(50, [20,20,10]),
		(100, [50,50]),
		(200, [100,100])
	     ]


nextLevel' :: [Integer] -> [Integer] -> [[Integer]]
nextLevel' _ []  = []
nextLevel' acc (1:xs) = nextLevel' (acc ++ [1]) xs
nextLevel' acc (x:xs) = compress $ sort (acc ++ reemplazo ++ xs) : nextLevel' (acc ++ [x]) xs 
				where reemplazo = maybe [] id (lookup x reemplazos)
compress = map head . group
