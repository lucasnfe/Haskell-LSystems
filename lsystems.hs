-- Haskell L-System
import System.Environment
import Graphics.Gloss
import Data.List.Split

-- data types
type Rule     = (Char, String)
type LSystem  = (Int, Float, String, [Rule])

consts :: String
consts = ['+', '-']

-- Parse L-Systems recursive step, it's the first (0) element in the list
parseStept :: [String] -> IO Int
parseStept grammarData = return (read (grammarData !! 0) :: Int)

-- Parse L-Systems angle, it's the second (1) element in the list
parseAngle :: [String] -> IO Float
parseAngle grammarData = return (read (grammarData !! 1) :: Float)

-- Parse L-Systems axiom, it's the third (2) element in the list
parseAxiom :: [String] -> IO String
parseAxiom grammarData = return (grammarData !! 2)

-- Parse L-Systems rules, they are all the elements after the second
breakRule :: String -> [String]
breakRule rule = splitOn "=" rule

createRule :: String -> Rule
createRule rule = (((breakRule rule) !! 0) !! 0, (breakRule rule) !! 1)

parse :: [String] -> [Rule]
parse [] = []
parse (x:y) = [createRule x] ++ (parse y)

getRules :: [String] -> IO [String]
getRules grammarData = return (take ((length grammarData) - 3) (reverse grammarData))

parseRules :: [String] -> IO [Rule]
parseRules grammarData = do
    rulesData <- getRules grammarData
    return (parse rulesData)

-- Parse L-Systems grammar, expects a list of strings and return an L-System
parseGrammarData :: [String] -> IO LSystem
parseGrammarData grammarData = do
    steps <- parseStept grammarData
    angle <- parseAngle grammarData
    axiom <- parseAxiom grammarData
    rules <- parseRules grammarData
    return (steps, angle, axiom, rules)

getGrammarLine :: String -> [String]
getGrammarLine s = lines s

parseGrammarFile :: String -> IO [String]
parseGrammarFile fileName = do
  text <- readFile fileName
  return (getGrammarLine text)

execRule :: Char -> Rule -> String
execRule x (pre, sec) = if x == pre then sec else []

execAllRules :: [Rule] -> Char -> String
execAllRules rules x = if x `elem` consts then [x] else execRule x =<< rules

replace :: LSystem -> String
replace (_, _, s, rs) = execAllRules rs =<< s

expandAxiom :: LSystem -> String
expandAxiom (0, _ , axiom, _) = axiom
expandAxiom ls@(i, angle, axiom, rules) = expandAxiom (i-1, angle, replace ls, rules)

showExpandedAxim :: LSystem -> IO String
showExpandedAxim ls@(i, angle, axiom, rules) = return (expandAxiom ls)

-- our program counts the number of spaces in input.txt
main :: IO ()
main = do
    c <- getArgs
    n <- parseGrammarFile (head c)
    s <- parseGrammarData n
    x <- showExpandedAxim s
    print x

-- Render the grammar
--  display
--      (InWindow "Gloss Line"
--      (400, 150)  -- window size
--      (10, 10)) 	 -- window position
--  white			 -- background color
--  picture		 -- picture to display
--
-- picture = line [
--  ( 50, 50),
--  ( 100, 100)]
