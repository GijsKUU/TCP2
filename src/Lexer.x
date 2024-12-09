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
  "left"                                    {\s -> TokenDir s}
  "right"                                   {\s -> TokenDir s}
  "front"                                   {\s -> TokenDir s}
  "go"                                      {\s -> TokenGo }
  "take"                                    {\s -> TokenTake}
  "mark"                                    {\s -> TokenMark}
  "nothing"                                 {\s -> TokenNothing_}
  "turn"                                    {\s -> TokenTurn}
  ";"                                       {\s -> TokenNextC}
  [\.\,]                                    { \s -> TokenSym (head s) }
  $beta [$alpha]+                           { \s -> TokenPattern s} 
  \_                                        { \s -> TokenPattern "_"}
  [$alpha $beta $digit $plusorminus]+       { \s -> TokenFunc s } 






