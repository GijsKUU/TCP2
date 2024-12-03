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
  [$alpha $beta $digit $plusorminus]+ $white+ "->"             { \s -> Identifier (Ident  s) } -- hier blijft de whitespace tussen de identifier en de '->' wel bewaard
  $white+                        ; 
  "--".*                         ;
  case                           { \s -> Case }
  of                             { \s -> Of }
  end                            { \s -> End }
  [\.\,]                         { \s -> Sym (head s) }
  $alpha+                        { \s -> Cmd s}
  $beta [$alpha]+                { \s -> Pattern s}
  \_                             { \s -> Pattern "_"}





