import UI.HSCurses.Curses

main = do
	initCurses
	cBreak True
	echo False

	scr <- initScr

	wclear scr

	keypad scr True
	mainLoop
	endWin

mainLoop = do
	   key <- getCh
	   evalKey key

evalKey (KeyChar 'q') 	= return ()
evalKey  k		= do
				putStrLn $show k
				mainLoop
