-- This assignment was made by Gijs Koppenberg (0779342) and Jason van Otterlo (...)
module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser
import ParseLib

import Data.Map (Map)
import qualified Data.Map as L

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive = undefined

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = undefined

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do

  chars <- readFile "examples/Test.arrow"
  --putStrLn "Input program:"
  --putStrLn ""
  --putStrLn chars
  --putStrLn ""
  --let tokens = alexScanTokens "turnAround  -> turn right, turn left."
  --putStrLn "Tokens - test 1 :"
  --putStrLn ""
  --print tokens

  let tokens = alexScanTokens chars
  --putStrLn "Tokens:"
  --putStrLn ""
  --print tokens
  let arr = parser tokens
  --putStrLn "Parsed program:"
  --putStrLn ""
  print arr

  -- Validate the parsed program using the checks
  putStrLn "Validation results:"
  validateProgram (Program arr)
  testSpacePrint
  testStep


testSpacePrint :: IO ()
testSpacePrint = do
  spaceString <- readFile "examples/AddInput.space"
  let space = parse parseSpace spaceString
  let space2 =head ( map fst space)
  putStrLn (printSpace space2)

testStep :: IO ()
testStep = do
  spaceString <- readFile "examples/Maze.space"
  let space = parse parseSpace spaceString
  let space2 =head ( map fst space)
  putStrLn (printSpace space2)
  script <- readFile "examples/Find.arrow"
  let solver = toEnvironment script
  let arrowstate = ArrowState space2 (0,0) South (solver L.! "start")
  putStrLn (solveMaze solver arrowstate)

solveMaze :: Environment -> ArrowState -> String
solveMaze env a = case s of
                      Ok newa -> solveMaze env newa
                      Done s p h -> printSpace s
                      Fail s -> s
                  where s = step env a


-- Validation function to check all conditions

validateProgram :: Program -> IO ()
validateProgram prog@(Program rules) = do

  putStrLn "\nValidation Results"
  putStrLn $ "Has start command? " ++ show (checkStartCmd rules)
  putStrLn $ "Are all rule calls valid? " ++ show (checkRuleCalls rules)
  putStrLn $ "Program has no duplicate rule definitions? " ++ show (checkDoubleDefined rules)
  putStrLn $ "Are all patterns matched? " ++ show (checkPatMatches rules)
  putStrLn $ "So is this program valid? " ++ show (checkProgram prog)


