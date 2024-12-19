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
interactive env state = do
    putStrLn "Current State:"
    putStrLn $ printSpace (getSpace state)
    putStrLn $ "Position: " ++ show (getPos state)
    putStrLn $ "Heading: " ++ show (getHeading state)
    putStrLn $ "Stack: " ++ show (getStack state)
    putStrLn "Press Enter to continue..."
    _ <- getLine
    
    case step env state of
        Done finalSpace finalPos finalHeading -> do
            putStrLn "Program completed successfully!"
            putStrLn $ printSpace finalSpace
            putStrLn $ "Final Position: " ++ show finalPos
            putStrLn $ "Final Heading: " ++ show finalHeading
        
        Ok nextState -> do
            interactive env nextState
        
        Fail errorMsg -> do
            putStrLn $ "Program failed: " ++ errorMsg
  where
    getSpace (ArrowState s _ _ _) = s
    getPos (ArrowState _ p _ _) = p
    getHeading (ArrowState _ _ h _) = h
    getStack (ArrowState _ _ _ s) = s

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env state = 
    case step env state of
        Done finalSpace finalPos finalHeading -> 
            (finalSpace, finalPos, finalHeading)
        
        Ok nextState -> 
            batch env nextState
        
        Fail errorMsg -> do
            let (ArrowState finalSpace finalPos finalHeading _) = state
            (finalSpace, finalPos, finalHeading)


{-
interactive :: Environment -> ArrowState -> IO ()
interactive = undefined

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = undefined

-}

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do

  --chars <- readFile "examples/Test.arrow"
  --putStrLn "Input program:"
  --putStrLn ""
  --putStrLn chars
  --putStrLn ""
  --let tokens = alexScanTokens "turnAround  -> turn right, turn left."
  --putStrLn "Tokens - test 1 :"
  --putStrLn ""
  --print tokens

  --let tokens = alexScanTokens chars
  --putStrLn "Tokens:"
  --putStrLn ""
  --print tokens
  --let arr = parser tokens
  --putStrLn "Parsed program:"
  --putStrLn ""
  --print arr

  -- Validate the parsed program using the checks
  --putStrLn "Validation results:"
  --validateProgram (Program arr)
  --testSpacePrint
  --testStep

  -- loading in script and space
  spaceString <- readFile "examples/SampleSpace.space"
  script <- readFile "examples/RemoveDebris.arrow"
  

  let space = head (map fst (parse parseSpace spaceString))
  let solver = toEnvironment script
  
  -- run interactive mode, start at 0,0 with "start"
  let arrowStateInteractive = ArrowState space (0,0) East (solver L.! "start")
  putStrLn "Interactive Mode"
  interactive solver arrowStateInteractive
  
  -- run batch mode
  let arrowStateBatch = ArrowState space (0,0) East (solver L.! "start")
  putStrLn "\nBatch Mode"
  let (finalSpace, finalPos, finalHeading) = batch solver arrowStateBatch
  putStrLn "Final State:"
  putStrLn $ printSpace finalSpace
  putStrLn $ "Final Position: " ++ show finalPos
  putStrLn $ "Final Heading: " ++ show finalHeading


testSpacePrint :: IO ()
testSpacePrint = do
  spaceString <- readFile "examples/AddInput.space"
  let space = parse parseSpace spaceString
  let space2 =head ( map fst space)
  putStrLn (printSpace space2)

testStep :: IO ()
testStep = do
  spaceString <- readFile "examples/SampleSpace.space"
  let space = parse parseSpace spaceString
  let space2 =head ( map fst space)
  putStrLn (printSpace space2)
  script <- readFile "examples/RemoveDebris.arrow"
  let solver = toEnvironment script
  let arrowstate = ArrowState space2 (0,0) South (solver L.! "start")
  let solved = solveMaze solver arrowstate
  putStrLn ("DONEEE")

solveMaze :: Environment -> ArrowState -> String
solveMaze env a = case s of
                      Ok newa -> solveMaze env newa
                      Done s p h -> printSpace s
                      Fail s -> s
                  where s = step env a


-- validation function to check all conditions
validateProgram :: Program -> IO ()
validateProgram prog@(Program rules) = do

  putStrLn "\nValidation Results"
  putStrLn $ "Has start command? " ++ show (checkStartCmd rules)
  putStrLn $ "Are all rule calls valid? " ++ show (checkRuleCalls rules)
  putStrLn $ "Program has no duplicate rule definitions? " ++ show (checkDoubleDefined rules)
  putStrLn $ "Are all patterns matched? " ++ show (checkPatMatches rules)
  putStrLn $ "So is this program valid? " ++ show (checkProgram prog)


