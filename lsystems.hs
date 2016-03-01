--
-- University of California, Santa Cruz --
--  Jack Baskin School of Engineering   --
--    Computer Science Department       --
--   CMPS203 - Pragramming Languages    --

-- Haskell L-System

-- Authors: Lucas N. Ferreira - lferreria@ucsc.edu --
--          Sumukh Atreya     - satreya@ucsc.edu   --

import System.Environment
import Graphics.Gloss
import Data.List.Split

-- data types
type Rule     = (Char, String)
type LSystem  = (Int, Float, String, [Rule])

type FloatVector2  = (Float, Float)

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

-- LSystems implementation
execRule :: Char -> Rule -> String
execRule x (pre, sec) = if x == pre then sec else []

execAllRules :: [Rule] -> Char -> String
execAllRules rules x = if x `elem` consts then [x] else execRule x =<< rules

replace :: LSystem -> String
replace (_, _, s, rs) = execAllRules rs =<< s

expandAxiom :: LSystem -> String
expandAxiom (0, _ , axiom, _) = axiom
expandAxiom ls@(i, angle, axiom, rules) = expandAxiom (i-1, angle, replace ls, rules)

showExpandedAxiom :: LSystem -> IO String
showExpandedAxiom ls@(i, angle, axiom, rules) = return (expandAxiom ls)

-- Rendering functions
-- nextLinePoint :: FloatVector2 -> Float -> FloatVector2
-- nextLinePoint curPoint dir = ((fst curPoint) + 10 * dir, (snd curPoint) + 10 * dir)
--
-- renderExpandedAxiom :: String -> [FloatVector2]
-- renderExpandedAxiom [] = []
-- renderExpandedAxiom (x:xs) = [nextLinePoint x] ++ (renderExpandedAxiom xs)

main :: IO ()
main = do

    -- Get the grammar filepath as an command line argument
    commandLineArgs <- getArgs

    -- Parse grammar file into a list of strings
    grammarData <- parseGrammarFile (head commandLineArgs)

    -- Create an LSystem using the grammar data described in the file
    lsystem <- parseGrammarData grammarData

    -- Apply LSystem expansion rules to get the result
    x <- showExpandedAxiom lsystem
    print x

    -- Render the results
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
