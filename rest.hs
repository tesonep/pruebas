{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}

import Yesod
import Control.Concurrent.MVar

data Prb = Prb { count:: MVar Int }

mkYesod "Prb" [$parseRoutes|
		/               Home    GET
	|]


instance Yesod Prb where approot _ = ""

getVar count = liftIO $ readMVar count
putVar count value = liftIO $ swapMVar count value

getHome = do
	Prb count <- getYesod
	cant <- getVar count
	putVar count (cant + 1)
	return $ RepPlain $ toContent (show cant)

main =  do
	x <- newMVar 0
	basicHandler 3000 (Prb x)
