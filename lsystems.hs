-- Haskell L-System
import System.Environment
import Graphics.Gloss

getGrammarLine :: String -> [String]
getGrammarLine s = lines s

parseGrammarFile :: String -> IO [String]
parseGrammarFile fileName = do
  text <- readFile fileName
  return (getGrammarLine text)

-- our program counts the number of spaces in input.txt
main :: IO ()
main = do
    c <- getArgs
    n <- parseGrammarFile (head c)
    print n
    
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
