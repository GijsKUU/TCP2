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
data Program = Program [Rule] deriving Show

type Func = String 

data Cmd 
    = GoCmd
    | TakeCmd
    | MarkCmd
    | NothingCmd
    | TurnCmd Dir
    | CaseOfCmd Dir Alt

data Dir = DirLeft | DirRight | DirFront deriving Show

data Rule = Rule Func [Cmd]

data Pat
    = EmptyPat
    | LabmdaPat
    | DebrisPat
    | AsteroidPat
    | BoundaryPat
    | UnderscorePat
    deriving (Show, Eq)

data Alt = Alt Pat [Cmd]






