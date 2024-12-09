{
module Parser where

import Model
}

%name parser
%tokentype { Token }
%error { happyError}

%token
  x         { Token }
  case      { TokenCase }
  of        { TokenOf }
  end       { TokenEnd }
  ','       { TokenSym ','}
  '.'       { TokenSym '.'}
  '->'      { TokenSymArrow }
  go        { TokenGo }
  take      { TokenTake }
  mark      { TokenMark }
  nothing   { TokenNothing_ }
  turn      { TokenTurn }
  ';'       { TokenNextC }
  Empty     { TokenPattern $$ }
  Lambda    { TokenPattern $$ }
  Debris    { TokenPattern $$ }
  Asteroid  { TokenPattern $$ }
  Boundary  { TokenPattern $$ }
  '_'       { TokenPattern $$ }
  left      { TokenDir $$ }
  right     { TokenDir $$ }
  front     { TokenDir $$ }
  func      { TokenFunc $$ }

%%


Program :: { [Rule] }
Program : Rule {[$1]}
        | Rule Program { $1 : $2 }


Rule :: { Rule } 
Rule : Func CmdList { Rule $1 $2 }

Func :: { Func}
Func : func { Functype $1}


Cmd :: { Cmd }
Cmd : go            {GoCmd}
    | take          {TakeCmd}
    | mark          {MarkCmd}
    | nothing       {NothingCmd}
    | turn Dir      {TurnCmd $2}
    | case Dir Alt  {CaseOfCmd $2 $3}

CmdList :: {[Cmd]}
CmdList : Cmd {[$1]}
        | Cmd CmdList { $1 : $2 }


Dir :: {Dir}
Dir : left  {DirLeft}
    | right {DirRight}
    | front {DirFront}


Pat :: {Pat}
Pat : Empty     {EmptyPat}
    | Lambda    {LambdaPat}
    | Debris    {DebrisPat}
    | Asteroid  {AsteroidPat}
    | Boundary  {BoundaryPat}
    | '_'         {UnderscorePat}

Alt :: {Alt}
Alt : Pat CmdList { Alt $1 $2 }




{

happyError _ = error "parse error"

}