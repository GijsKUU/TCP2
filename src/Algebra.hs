module Algebra where

import Model
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace



-- Exercise 5

-- pr :: Program, r :: Rule, c :: Cmd
data Algebra pr r c = Algebra {
    prF :: [r] -> pr, -- Program
    rF :: Func -> [c] -> r, -- Rule
    cF :: c} -- Cmd

fold :: Algebra pr r c -> Program -> pr
fold (Algebra mapPr mapR mapC) = foldProgram
    where
        foldProgram (Program rules) = mapPr (map foldRule rules)
        -- map the foldRule on the [Rule]('rules') in Program
        foldRule (Rule func cmds) = mapR func (map foldCmd cmds)
        -- map foldCmd on the commands in Rule
        foldCmd _ = mapC 
        -- do mapC

-- Exercise 6

-- main function to call to check program validity
check :: Program -> Bool
check program = checkProgram program


checkProgram :: Program -> Bool
checkProgram (Program rules) = 
    checkStartCmd  rules && 
    checkRuleCalls rules &&
    checkDoubleDefined  rules && 
    checkPatMatches rules  


-- (1) check if the program has a start command

-- Check if it has start command by folding over program
checkStartCmd :: ProgramAlgebra Bool
checkStartCmd rules = foldProgram (\rules -> any (\(Rule (Functype name) _) -> name == "start") rules) (Program rules)



-- (2) check if all calles to rules are made to rules that actually exist
checkRuleCalls :: ProgramAlgebra Bool
checkRuleCalls rules = 
    let ruleNames = foldProgram (map (\(Rule (Functype name) _) -> name)) (Program rules) -- extracting all rule names
        calledRules = foldProgram (concatMap (\(Rule _ cmds ) -> returnRuleCall cmds)) (Program rules)
    in Set.isSubsetOf (Set.fromList calledRules) (Set.fromList ruleNames)


-- checks whether command is a call to a function, or rule
returnRuleCall :: [Cmd] -> [String]
returnRuleCall [] = []
returnRuleCall (cmd:xs) = 
    case cmd of
        FuncCmd (Functype name) -> name : returnRuleCall xs ++ returnRuleCallNested cmd
        CaseOfCmd _ alts -> 
            returnRuleCallAlts alts ++ returnRuleCall xs
        _ -> returnRuleCall xs

-- since the parser returns nested inputs, some of the rule calls are in an Alt
returnRuleCallAlts :: [Alt] -> [String]
returnRuleCallAlts [] = []
returnRuleCallAlts (Alt _ cmds:xs) = returnRuleCall cmds ++ returnRuleCallAlts xs

-- checking a nested Case of command to check if they contain rule calls
returnRuleCallNested :: Cmd -> [String]
returnRuleCallNested cmd = 
    case cmd of
        CaseOfCmd _ alts -> returnRuleCallAlts alts
        _ -> []

getAllRuleNames :: ProgramAlgebra [String]
getAllRuleNames rules = map (\(Rule (Functype name) _) -> name) rules



-- (3) check if all rules are defined exactly ones
checkDoubleDefined :: ProgramAlgebra Bool -- if the size of the union of itself is the same size as the normal set there is no double values
checkDoubleDefined rules = Set.size setrules == length rulesList
    where
        setrules = Set.fromList (getAllRuleNames rules)
        rulesList = getAllRuleNames rules






-- (4) no ability to pattern match failure

-- fold over rules to see if any has an error in the pattern matching
checkPatMatches :: [Rule] -> Bool
checkPatMatches [] = True
checkPatMatches ((Rule func cmds) : xs) = noPatMatchFail cmds && checkPatMatches xs

noPatMatchFail :: [Cmd] -> Bool
noPatMatchFail [] = True
noPatMatchFail ((CaseOfCmd d a) : xs) = allPatsMatched (CaseOfCmd d a) && noPatMatchFail xs
noPatMatchFail (_ : xs) = True && noPatMatchFail xs



allPatsMatched :: Cmd -> Bool
allPatsMatched (CaseOfCmd dir alts) = needed == pats || Set.member UnderscorePat pats
    where
        needed = Set.fromList ([EmptyPat, LambdaPat, DebrisPat, AsteroidPat, BoundaryPat, UnderscorePat])
        pats = Set.fromList (getPatFromAlts alts)



