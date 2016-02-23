-- | Display "Hello World" in a window.
--
import Graphics.Gloss

main
 = display
        (InWindow "Gloss Line"
		(400, 150) 	 -- window size
		(10, 10)) 	 -- window position
	white			 -- background color
	picture			 -- picture to display

picture = line [
    (-50, -100),
    ( 100, 200)]
