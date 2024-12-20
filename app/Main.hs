-- This assignment was made by Gijs Koppenberg (0779342) and Jason van Otterlo (9425268)
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


main :: IO ()
main = do
  -- loading in script and space
  spaceString <- readFile "examples/AddInput.space"
  script <- readFile "examples/Add.arrow"
  
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
  putStrLn solved
  putStrLn ("DONEEE")

solveMaze :: Environment -> ArrowState -> String
solveMaze env a = case s of
                      Ok newa -> solveMaze env newa
                      Done s p h -> printSpace s
                      Fail s -> s
                  where s = step env a

