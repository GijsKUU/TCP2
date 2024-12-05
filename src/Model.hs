module Model where

-- Exercise 1
data Token 
    = Case
    | Of
    | End
    | Sym Char -- symbols: '.' & ','
    | SymArrow
    | Go
    | Take
    | Mark
    | Nothing_
    | Turn
    | Token -- placeholder
    | NextC
    | Pattern String -- Empty | Lambda | Debris | Asteroid | Boundary | _
    | Dir String
    | Func String -- function names
    deriving Show







-- Exercise 2 
-- "A program is a sequence of rules"
-- "This needs to be turned into a real abstract language"

data Program = Program deriving Show

{-
data Program = [Rule] deriving Show

data Rule = Func -> Cmds

data Cmds = Cmd | Cmd ("," Cmd) 

data Cmd 
    = Go
    | Take
    | Mark
    | Nothing_
    | Turn Dir 
    | Case Dir Of Alts End
    | Func


data Dir = Dir Left | Dir Right | Dir Front

data Alts = Alt | Alt (";" Alt)

data Alt = Pattern -> Cmds

data Pattern = Empty | Lambda | Debris | Asteroid | Boundary | _

-}


