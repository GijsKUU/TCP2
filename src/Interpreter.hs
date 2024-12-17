module Interpreter where
 
import ParseLib
 
import Data.Map (Map)
import qualified Data.Map as L
 
import Data.Char (isSpace)
import Control.Monad (replicateM)
 
import Lexer
import Parser
import Model
import Algebra
 
 
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Ord)
 
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
printField space ([],[]) css = []
printField space ((r:rs), []) css = "\n" ++ printField space (rs, css) css
printField space ((r:rs),(c:cs)) css = getContent (space L.! (r,c)) contentsTable : printField space (r:rs,cs) css
 
getContent :: Contents -> [(Contents, Char)] -> Char
getContent content (c:cs) | content == fst c = snd c
                          | otherwise = getContent content cs
 
-- These three should be defined by you
type Ident = String -- I think Ident is a rule name
type Commands = [Cmd]   -- and commands is just a list of commands again 
type Heading = Heads -- which way are we currently headed
 
data Heads = North | East | South | West

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
step = undefined