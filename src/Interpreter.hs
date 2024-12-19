module Interpreter where
 
import ParseLib
 
import Data.Map (Map)
import qualified Data.Map as L

import Debug.Trace (trace)
 
import Data.Char (isSpace)
import Control.Monad (replicateM)
 
import Lexer
import Parser
import Model
import Algebra

data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Ord, Show)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents
 
 
 
-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs -> zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)
 
    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces
 
 
-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]
 
 
-- Exercise 7
printSpace :: Space -> String
printSpace space = do
        let (rows, columns) = L.foldrWithKey (\(keyr, keyc) value (mr, mc) -> (max keyr mr, max keyc mc)) (0,0) space
            rcString = '(' : show rows ++ ',' : show columns ++ ")\n"
            rowList = [0..rows]
            columnList = [0..columns]
            restString = printField space (rowList, columnList) columnList

        rcString ++ restString

printField :: Space -> ([Int], [Int]) -> [Int]-> String -- ([0..rows], [0..columns]) [0..columns]
printField space ([],_) css = []
printField space ((r:rs), []) css = "\n" ++ printField space (rs, css) css
printField space ((r:rs),(c:cs)) css = getContent (space L.! (r,c)) contentsTable : printField space (r:rs,cs) css

getContent :: Contents -> [(Contents, Char)] -> Char
getContent content (c:cs) | content == fst c = snd c
                          | otherwise = getContent content cs

-- These three should be defined by you
type Ident = String -- I think Ident is a rule name
type Commands = [Cmd]   -- and commands is just a list of commands again 
type Heading = Heads -- which way are we currently headed
 
data Heads = North | East | South | West deriving Show

type Environment = Map Ident Commands
 
type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack
 
data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment input = do
  let tokens = alexScanTokens input
  let parsed = parser tokens
  let rules = L.fromList [(name, cmds) | (Rule (Functype name) cmds) <- parsed]
  if check (Program parsed) then rules else L.fromList []
    

 
-- | Exercise 9
step :: Environment -> ArrowState -> Step
step env (ArrowState s p h []) = Done s p h
step env (ArrowState s p h (GoCmd:cs)) = Ok (ArrowState s (moveForward h p s) h cs)
step env (ArrowState s p h (TakeCmd:cs)) = let space = L.insert p Empty s in Ok (ArrowState space p h cs)
step env (ArrowState s p h (MarkCmd:cs)) = let space = L.insert p Lambda s in Ok (ArrowState space p h cs)
step env (ArrowState s p h (NothingCmd:cs)) = Ok (ArrowState s p h cs)
step env (ArrowState s p h (TurnCmd d:cs)) = Ok (ArrowState s p (makeTurn h d) cs)
step env a@(ArrowState s p h (CaseOfCmd d as:cs)) = sensorRead a
step env a@(ArrowState s p h (FuncCmd f:cs)) = ruleCall a env

--GoCmd
moveForward :: Heading -> Pos -> Space -> Pos
moveForward North (y, x) s | not (L.member (y-1, x) s) || s L.! (y-1, x) == Asteroid || s L.! (y-1, x) == Boundary = (y, x) 
                           | otherwise = (y-1, x)
moveForward East (y, x) s  | not (L.member (y, x+1) s) || s L.! (y, x+1) == Asteroid || s L.! (y, x+1) == Boundary = (y, x) 
                           | otherwise = (y, x+1)
moveForward South (y, x) s | not (L.member (y+1, x) s) || s L.! (y+1, x) == Asteroid || s L.! (y+1, x) == Boundary = (y, x) 
                           | otherwise = (y+1, x)
moveForward West (y, x) s  | not (L.member (y, x-1) s) || s L.! (y, x-1) == Asteroid || s L.! (y, x-1) == Boundary = (y, x) 
                           | otherwise = (y, x-1)

-- turning, hardcoded
makeTurn :: Heading -> Dir -> Heading
makeTurn North DirRight = East
makeTurn North DirLeft  = West
makeTurn East DirRight  = South
makeTurn East DirLeft   = North
makeTurn South DirRight = West
makeTurn South DirLeft  = East
makeTurn West DirRight  = North
makeTurn West DirLeft   = South
makeTurn h DirFront     = h

--CaseOfCmd
sensorRead :: ArrowState -> Step
sensorRead (ArrowState s p h (CaseOfCmd d as:cs)) = case iterateAlts scanRes as of 
                                                      Just cmds -> Ok (ArrowState s p h (cmds++cs))
                                                      Nothing -> Fail ("No Alternative matches found on pattern: " ++ show scanRes)
                                                    where scanRes = scan s p (makeTurn h d)

scan :: Space -> Pos -> Heading -> Contents
scan s (y, x) North | not (L.member (y-1, x) s) = Boundary
                    | otherwise = s L.! (y-1, x)
scan s (y, x) East  | not (L.member (y, x+1) s) = Boundary
                    | otherwise = s L.! (y, x+1)
scan s (y, x) South | not (L.member (y+1, x) s) = Boundary
                    | otherwise = s L.! (y+1, x)
scan s (y, x) West  | not (L.member (y, x-1) s) = Boundary
                    | otherwise = s L.! (y, x-1)

iterateAlts :: Contents -> [Alt] -> Maybe [Cmd]
iterateAlts c [] = Nothing
iterateAlts c ((Alt pat cs):as) | pat == UnderscorePat = Just cs
                                | c == patToContents pat = Just cs
                                | otherwise = iterateAlts c as
                              
patToContents :: Pat -> Contents
patToContents EmptyPat = Empty
patToContents LambdaPat = Lambda
patToContents DebrisPat = Debris
patToContents AsteroidPat = Asteroid
patToContents BoundaryPat = Boundary
--patToContents UnderscorePat = Empty -- placeholder

--FuncCmd
ruleCall :: ArrowState -> Environment -> Step
ruleCall (ArrowState s p h (FuncCmd f@(Functype str):cs)) env = case getCmds env f of
                                                    Just cmds -> Ok (ArrowState s p h (cmds++cs))
                                                    Nothing -> Fail ("Rule in rulecall not defined: " ++ str)

getCmds :: Environment -> Func -> Maybe [Cmd]
getCmds env (Functype str) | not (L.member str env) = Nothing
                           | otherwise = Just (env L.! str)