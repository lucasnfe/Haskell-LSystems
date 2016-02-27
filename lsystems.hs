-- Haskell L-System
import System.Environment
import Graphics.Gloss

main = do

    -- Get the file path from command line
    c <- getArgs
    print c

    -- Read the file passed as argument
    f <- readFile (head c)

    -- Parse the file

    -- Render the grammar
    display
        (InWindow "Gloss Line"
		(400, 150) 	 -- window size
		(10, 10)) 	 -- window position
	white			 -- background color
	picture			 -- picture to display

picture = line [
    ( 50, 50),
    ( 100, 100)]
