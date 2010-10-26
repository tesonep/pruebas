data Persona = Persona { 
				nombre::String, 
				apellido::String, 
				edad::Int
				} deriving Show

cumplirAnhos (Persona n a e) = Persona n a (e+1)

nCumpleanhos n x = foldl (\a _ -> cumplirAnhos a) x [1..n]