module Algebra where

import Model
import Data.Set (Set)
import qualified Data.Set as Set



-- Exercise 5
type Algebra r c d p a = ( -- r :: Rule, c :: Cmd, d :: Dir, p :: Pat, a :: Alt
    [r] -> r, -- Program
    Func -> [c] -> r, -- Rule
    c, --(GoCmd, TakeCmd, MarkCmd, NothingCmd)
    d -> c, --TurnCmd
    d -> [a] -> c, --CaseOfCmd
    Func -> c, --FuncCmd
    Dir -> d, --Dir
    Pat -> [c] -> a, -- Alt
    Pat -> p ) -- pat

fold :: Algebra r c d p a -> Program -> r
fold (mapPr, mapR, mapC, mapTC, mapCoC, mapFC, mapD, mapA, mapP) = foldProgram
    where
        foldProgram (Program rules) = mapPr (map foldRule rules)
        -- map the foldRule on the [Rule]('rules') in Program

        foldRule (Rule f cs) = mapR f (map foldCmd cs)
        -- map foldCmd on [Cmd] and do mapR


        foldCmd GoCmd = mapC
        foldCmd TakeCmd = mapC
        foldCmd MarkCmd = mapC
        foldCmd NothingCmd = mapC
        foldCmd (TurnCmd d) = mapTC (foldDir d)
        foldCmd (CaseOfCmd d as) = mapCoC (foldDir d) (map foldAlt as)
        foldCmd (FuncCmd f) = mapFC f
        
        foldDir dir = mapD dir

        foldAlt (Alt p cs) = mapA p (map foldCmd cs)

        foldPat pat = mapP pat
   






-- Exercise 6

-- main function to call to check program validity
checkProgram :: Program -> Bool
checkProgram program = let
    finalState = fold checkAlgebra program initialState
    in
        allChecksPass finalState

data ProgramInfo = ProgramInfo {
    definedRules :: Set Func,   -- set of all defined rules
    calledRules :: Set Func,    -- set of all called rules 
    hasStart :: Bool,           -- has a start rule or not
    hasDuplicates :: Bool,      -- has duplicate rules
    noPatMatchFail :: Bool      -- no pattern matches can fail
} deriving Show 



-- type CheckAlgebra = Algebra ProgramInfo () Dir () ()

initialState :: ProgramInfo
initialState = ProgramInfo Set.empty Set.empty False False True


checkAlgebra :: Algebra (ProgramInfo -> ProgramInfo) () Dir () ()
checkAlgebra = (
    checkProgram2,          -- all rules are correct
    checkRule,              -- single rule follows constraints
    (),                     -- normal commands don't have to be checked  (goCmd, takeCmd, etc.)
    \_ -> (),               -- turnCmd doesn't need to be checken
    checkCaseOfCmd,         -- checking constraint that all case ofs need to pattern match all 5 cases or have a catch all
    \_ -> (),     -- keep track of all calls to functions that are made
    id,                     -- just keep direction 
    checkAlt,               -- check al Alt patterns
    id                      -- keep pattern
    )

checkProgram2 :: [Rule] -> ProgramInfo -> ProgramInfo
checkProgram2 rules state = foldl (flip checkRule) state rules


checkRule :: Rule -> ProgramInfo -> ProgramInfo
checkRule (Rule f cmds) info = 
    let state = 
        -- if we find a "start" rule anywhere we turn hasStart to True
            if f == Functype "start" then info { hasStart = True } else info
        -- if the current rule is an already defined rule, we have duplicates
        state2 = 
            if Set.member f (definedRules state) then state {hasDuplicates = True} else state {definedRules = Set.insert f (definedRules state)} -- if the current rule isn't already defined, add it to the set of defined rules
        in foldl (flip checkCmd) state2 cmds



checkCaseOfCmd :: Cmd -> ProgramInfo -> ProgramInfo
checkCaseOfCmd (FuncCmd f) info = 
    info {calledRules = Set.insert f (calledRules info)} -- keeping track of called rules, to see if all rules get called
checkCaseOfCmd (CaseOfCmd _ alts) info = 
    let 
        patterns = map (\(Alt p _) -> p) alts -- get a list of all patterns, ignoring following commands
        validPatterns = checkPatterns patterns
    in 
        info { noPatMatchFail = noPatMatchFail info && validPatterns }
checkCmd _ info = info

-- check if all patterns are matched or the catch all is used 
checkPatterns :: [Pat] -> Bool
checkPatterns patterns = Set.isSubsetOf neededPats pats ||  elem UnderscorePat patterns -- are all needed patterns matched, or is the '_' catch all there
    where
        neededPats = Set.fromList [EmptyPat, LambdaPat, DebrisPat, AsteroidPat, BoundaryPat]
        pats = Set.fromList patterns


-- processing alts, used for processing commands
checkAlt :: Alt -> ProgramInfo -> ProgramInfo
checkAlt (Alt _ cmds) info = foldl (flip checkCaseOfCmd) info cmds



allChecksPass :: ProgramInfo -> Bool
allChecksPass info =
    hasStart info &&
    not (hasDuplicates info) &&
    Set.isSubsetOf (calledRules info) (definedRules info) &&
    noPatMatchFail info



        


