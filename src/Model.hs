module Model where

-- Exercise 1
data Token 
    = TokenCase
    | TokenOf
    | TokenEnd
    | TokenSym Char -- symbols: '.' & ','
    | TokenSymArrow
    | TokenGo
    | TokenTake
    | TokenMark
    | TokenNothing_
    | TokenTurn
    | Token -- placeholder
    | TokenNextC -- ';'
    | TokenPattern String -- Empty | Lambda | Debris | Asteroid | Boundary | _
    | TokenDir String
    | TokenFunc String -- function names
    deriving Show







-- Exercise 2 
-- "A program is a sequence of rules"
data Program = Program [Rule] deriving Show

data Rule = Rule Func [Cmd] deriving Show

newtype Func = Functype String deriving Show

newtype SymArrow = ArrowType Token deriving Show

data Cmd 
    = GoCmd
    | TakeCmd
    | MarkCmd
    | NothingCmd
    | TurnCmd Dir
    | CaseOfCmd Dir [Alt]
    | FuncCmd Func
    deriving Show

data Dir = DirLeft | DirRight | DirFront deriving Show

data Alt = Alt Pat [Cmd] deriving Show

data Pat
    = EmptyPat
    | LambdaPat
    | DebrisPat
    | AsteroidPat
    | BoundaryPat
    | UnderscorePat
    deriving (Show, Eq)







