-- Haskell L-System
import System.Environment
import Graphics.Gloss

-- data types
type Rule     = (Char, String)
type LSystem  = (Int, Float, String, [Rule])

consts :: String
consts = ['+', '-']

getGrammarLine :: String -> [String]
getGrammarLine s = lines s

parseGrammarFile :: String -> IO [String]
parseGrammarFile fileName = do
  text <- readFile fileName
  return (getGrammarLine text)

execRule :: Char -> Rule -> String
execRule x (pred, sec) = if x == pred then sec else []

execAllRules :: [Rule] -> Char -> String
execAllRules rules x = if x `elem` consts then [x] else execRule x =<< rules

replace :: LSystem -> String
replace (_ ,_ ,s ,rs) = execAllRules rs =<< s

expandAxiom :: LSystem -> String
expandAxiom (0, _ , axiom, _) = axiom
expandAxiom (i, angle, axiom, rules) = expandAxiom (i-1, angle, replace (i, angle, axiom, rules), rules)

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
