module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser

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
  putStrLn "Input program:"
  putStrLn ""
  putStrLn chars
  putStrLn ""
  --let tokens = alexScanTokens "turnAround  -> turn right, turn left."
  --putStrLn "Tokens - test 1 :"
  --putStrLn ""
  --print tokens
  let tokens = alexScanTokens chars
  putStrLn "Tokens:"
  putStrLn ""
  print tokens
  let arr = parser tokens
  putStrLn "Parsed program:"
  putStrLn ""
  print arr

  -- Validate the parsed program using the checks
  putStrLn "Validation results:"
  validateProgram (Program arr)



-- Validation function to check all conditions
validateProgram :: Program -> IO ()
validateProgram prog = do
  putStrLn $ "Has start command? " ++ show (checkStartCmd prog)
  putStrLn $ "Are all rule calls valid? " ++ show (checkRuleCalls prog)
  putStrLn $ "Program has no duplicate rule definitions? " ++ show (checkDoubleDefined prog)
  putStrLn $ "Are all patterns matched? " ++ show (checkPatMatches (getRules prog))
  putStrLn $ "So is this program valid? " ++ show (checkProgram prog)

-- Helper to extract the list of rules from a program
getRules :: Program -> [Rule]
getRules (Program rules) = rules
