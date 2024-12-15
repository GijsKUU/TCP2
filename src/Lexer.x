{
module Lexer where

import Model
}

%wrapper "basic"
$alpha  = [a-z] -- lowercase characters
$beta   = [A-Z] -- uppercase characters 
$digit  = 0-9
$plusorminus = [\+\-]
--$name   = ($alpha | $beta | $digit | $plusorminus)+ 


tokens :-
  $white+                                   ; 
  "--".*                                    ; 
  "->"                                      {\s -> TokenSymArrow}                                     
  "case"                                    { \s -> TokenCase }
  "of"                                      { \s -> TokenOf }
  "end"                                     { \s -> TokenEnd }
  "left"                                    {\s -> TokenLeft}
  "right"                                   {\s -> TokenRight}
  "front"                                   {\s -> TokenFront}
  "go"                                      {\s -> TokenGo }
  "take"                                    {\s -> TokenTake}
  "mark"                                    {\s -> TokenMark}
  "nothing"                                 {\s -> TokenNothing_}
  "turn"                                    {\s -> TokenTurn}
  \;                                        {\s -> TokenNextC}
  \.                                        { \s -> TokenPeriod }
  \,                                        { \s -> TokenComma }
  "Empty"                                   { \s -> TokenEmptyPat}
  "Lambda"                                  { \s -> TokenLambdaPat}
  "Debris"                                  { \s -> TokenDebrisPat}
  "Asteroid"                                { \s -> TokenAsteroidPat}
  "Boundary"                                { \s -> TokenBoundaryPat} 
  \_                                        { \s -> TokenUnderscorePat}
  [$alpha $beta $digit $plusorminus]+       { \s -> TokenFunc s } 






