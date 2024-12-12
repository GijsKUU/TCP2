module Algebra where

import Model


-- Exercise 5
type Algebra r c d p a = ( -- r :: Rule, c :: Cmd, d :: Dir, p :: Pat, a :: Alt
    [r] -> r, -- Program
    Func -> [c] -> r, -- Rule
    c, --(GoCmd, TakeCmd, MarkCmd, NothingCmd)
    d -> c, --TurnCmd
    d -> [a] -> c, --CaseOfCmd
    Func -> c, --FuncCmd
    d, --Dir
    p -> [c] -> a, -- Alt
    p ) -- pat

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
        
        foldDir DirLeft = mapD
        foldDir DirRight = mapD
        foldDir DirFront = mapD

        foldAlt (Alt p cs) = mapA (foldPat p) (map foldCmd cs)

        foldPat EmptyPat = mapP
        foldPat LambdaPat = mapP
        foldPat DebrisPat = mapP
        foldPat AsteroidPat = mapP
        foldPat BoundaryPat = mapP
        foldPat UnderscorePat = mapP






-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined