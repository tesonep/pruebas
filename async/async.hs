import System.Directory


filtro x = x == "." || x == ".."
getDirectoryContent' dir = (getDirectoryContents dir)
