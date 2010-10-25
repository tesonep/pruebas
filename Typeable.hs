import Data.Typeable
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Data
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams

class (Data a, Typeable a, Show a) => Todos a where
   haceTuMagia :: a -> String

data Unos = Unos deriving (Show,Typeable,Data)
   
instance Todos Unos where
   haceTuMagia _ = "unos"
 
data Otros = Otros {nombre::String, apellido::String} deriving (Show,Typeable,Data)
 
instance Todos Otros where
   haceTuMagia _ = "otros"
   
data TodosWrapper = forall a. Todos a => TW a
   
algo :: [TodosWrapper] -> [String]
algo [] = []
algo (TW x:xs) = haceTuMagia x : algo xs
	

