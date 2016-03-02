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
steps :: LSystem ->  Int
steps (s, _, _, _) = s

angle :: LSystem -> Float
angle (_, a, _, _) = a

axiom :: LSystem -> String
axiom (_, _, x, _) = x

rules :: LSystem -> [Rule]
rules (_, _, _, r) = r

execRule :: Char -> Rule -> String
execRule x (pre, sec) = if x == pre then sec else []

execAllRules :: [Rule] -> Char -> String
execAllRules rules x = if x `elem` consts then [x] else execRule x =<< rules

replace :: LSystem -> String
replace (_, _, s, rs) = execAllRules rs =<< s

expandAxiom :: LSystem -> String
expandAxiom (0, _ , axiom, _) = axiom
expandAxiom ls@(i, angle, axiom, rules) = expandAxiom (i-1, angle, replace ls, rules)

-- Rendering functions
degToRad :: Float -> Float
degToRad alpha = alpha * (pi / 180)

rotateVector :: FloatVector2 -> Float -> FloatVector2
rotateVector v alpha = ((cos alpha) * (fst v) - (sin alpha) * (snd v),
                        (sin alpha) * (fst v) + (cos alpha) * (snd v))

nextLinePoint :: FloatVector2 -> FloatVector2 -> Float -> FloatVector2
nextLinePoint curPoint direc dist = ((fst curPoint) + dist * (fst direc),
                                     (snd curPoint) + dist * (snd direc))

renderExpandedAxiom :: String -> FloatVector2 -> FloatVector2 -> Float -> Float -> [FloatVector2]
renderExpandedAxiom [] _ _ _ _ = []
renderExpandedAxiom ('-':xs) startPoint direc alpha dist = renderExpandedAxiom xs startPoint (rotateVector direc (0.0 - alpha)) alpha dist
renderExpandedAxiom ('+':xs) startPoint direc alpha dist = renderExpandedAxiom xs startPoint (rotateVector direc alpha) alpha dist
renderExpandedAxiom ('F':xs) startPoint direc alpha dist = [nextLinePoint startPoint direc dist] ++ (renderExpandedAxiom xs (nextLinePoint startPoint direc dist) direc alpha dist)
renderExpandedAxiom (_:xs) startPoint direc alpha dist = renderExpandedAxiom xs startPoint direc alpha dist

main :: IO ()
main = do

    -- Get the grammar filepath as an command line argument
    commandLineArgs <- getArgs

    -- Parse grammar file into a list of strings
    grammarData <- parseGrammarFile (head commandLineArgs)

    -- Create an LSystem using the grammar data described in the file
    lsystem <- parseGrammarData grammarData

    -- Apply LSystem expansion rules to get the result
    dist <- return 5.0
    direc <- return (0.0, 1.0)
    startPoint <- return (0.0, 0.0)

    alpha  <- return (degToRad (angle lsystem))
    result <- return (expandAxiom lsystem)

    points  <- return (renderExpandedAxiom result startPoint direc alpha dist)
    picture <- return (line points)

    -- Render the results
    display (InWindow "Haskell L-System" (800, 600) (0, 0)) white picture
