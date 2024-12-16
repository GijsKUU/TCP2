module Algebra where

import Model
import Data.Set (Set)
import qualified Data.Set as Set



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

checkProgram :: Program -> Bool
checkProgram program = allChecksPass state
        where state = fold checkAlgebra program


data ProgramInfo = ProgramInfo {
    definedRules :: Set Func,   -- set of all defined rules
    calledRules :: Set Func,    -- set of all called rules 
    hasStart :: Bool,           -- has a start rule or not
    hasDuplicates :: Bool,      -- has duplicate rules
    noPatMatchFail :: Bool      -- no pattern matches can fail
} deriving Show 

initialState :: ProgramInfo
initialState = ProgramInfo Set.empty Set.empty False False True

allChecksPass :: ProgramInfo -> Bool
allChecksPass info =
    hasStart info &&
    not (hasDuplicates info) &&
    Set.isSubsetOf (calledRules info) (definedRules info) &&
    noPatMatchFail info


checkAlgebra :: Algebra ProgramInfo --Program
                        (ProgramInfo -> ProgramInfo) --Rule
                        (ProgramInfo -> ProgramInfo) --Cmd
checkAlgebra = Algebra {
    prF = programCheck . foldl(\ac c -> c ac) initialState,
    rF = \f commands info -> ruleCheck f info, 
    cF = id
}

programCheck :: ProgramInfo -> ProgramInfo
programCheck = undefined

ruleCheck :: Func -> ProgramInfo -> ProgramInfo
ruleCheck f@(Func str) info = ProgramInfo {definedRules = newRules, hasDuplicates = unique, hasStart = hasstart}
            where newRules = Set.insert f (definedRules info)
                  unique = Set.member f (definedRules info) || hasDuplicates info
                  hasstart = str == "start" || hasStart info












