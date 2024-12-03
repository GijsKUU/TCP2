module Model where

-- Exercise 1
data Token 
    = Case
    | Of
    | End
    | Sym Char -- symbols: '.' & ','
    | Cmd String  -- cmds: go, take, mark, nothing, turn, case, of, end, left, right, front, ;
    | Pattern String -- patterns: Empty, Lambda, Debris, Asteroid, Boundary, _
    | Identifier Ident
    deriving Show

newtype Ident = Ident String deriving Show






-- Exercise 2 
-- "A program is a sequence of rules"
data Program = Program deriving Show
