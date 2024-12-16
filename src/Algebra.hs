module Algebra where

import Model
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace




-- Exercise 5

type ProgramAlgebra r = ([Rule] -> r)

type RuleAlgebra r1 r2 = (Func -> [Cmd] -> r2)

type FuncAlgebra r = (String -> r)

type CmdAlgebra r1 r2 r3 r4 r5 r6 r7 = (
    r1,                   -- GoCmd
    r1,                   -- TakeCmd
    r1,                   -- MarkCmd
    r1,                   -- NothingCmd
    Dir -> r1,            -- TurnCmd
    Dir -> [Alt] -> r1,   -- CaseOfCmd
    Func -> r1            -- FuncCmd
    )

type DirAlgebra r = (r, r, r)  -- left right front

type AltAlgebra r1 r2 = (Pat -> [Cmd] -> r2)

type PatAlgebra r = (
    r,  -- empty
    r,  -- lambda
    r,  -- debris
    r,  -- asteroid
    r,  -- boundary
    r   -- underscore
    )


foldProgram :: ProgramAlgebra r -> Program -> r
foldProgram f (Program rules) = f rules

foldRule :: RuleAlgebra r1 r2 -> Rule -> r2
foldRule f (Rule func cmds) = f func cmds

foldFunc :: FuncAlgebra r -> Func -> r
foldFunc f (Functype str) = f str

foldCmd :: CmdAlgebra r1 r2 r3 r4 r5 r6 r7 -> Cmd -> r1
foldCmd (go, take, mark, nothing, turn, caseof, func) cmd = 
    case cmd of
        GoCmd -> go
        TakeCmd -> take
        MarkCmd -> mark
        NothingCmd -> nothing
        TurnCmd dir -> turn dir
        CaseOfCmd dir alts -> caseof dir alts
        FuncCmd f -> func f

foldDir :: DirAlgebra r -> Dir -> r
foldDir (left, right, front) dir =
    case dir of
        DirLeft -> left
        DirRight -> right
        DirFront -> front

foldAlt :: AltAlgebra r1 r2 -> Alt -> r2
foldAlt f (Alt pat cmds) = f pat cmds

foldPat :: PatAlgebra r -> Pat -> r
foldPat (empty, lambda, debris, asteroid, boundary, underscore) pat =
    case pat of
        EmptyPat -> empty
        LambdaPat -> lambda
        DebrisPat -> debris
        AsteroidPat -> asteroid
        BoundaryPat -> boundary
        UnderscorePat -> underscore



-- Exercise 6

-- main function to call to check program validity
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

    
getPatFromAlts :: [Alt] -> [Pat]
getPatFromAlts alts = map (\(Alt pat _) -> pat) alts








        


