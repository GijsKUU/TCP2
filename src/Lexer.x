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
  "->"                                      {\s -> SymArrow}                                     
  "case"                                    { \s -> Case }
  "of"                                      { \s -> Of }
  "end"                                     { \s -> End }
  "left"                                    {\s -> Dir s}
  "right"                                   {\s -> Dir s}
  "front"                                   {\s -> Dir s}
  "go"                                      {\s -> Go }
  "take"                                    {\s -> Take}
  "mark"                                    {\s -> Mark}
  "nothing"                                 {\s -> Nothing_}
  "turn"                                    {\s -> Turn}
  ";"                                       {\s -> NextC}
  [\.\,]                                    { \s -> Sym (head s) }
  $beta [$alpha]+                           { \s -> Pattern s} 
  \_                                        { \s -> Pattern "_"}
  [$alpha $beta $digit $plusorminus]+       { \s -> Func s } 






