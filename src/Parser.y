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
  ','       { TokenComma}
  '.'       { TokenPeriod}
  '->'      { TokenSymArrow }
  go        { TokenGo }
  take      { TokenTake }
  mark      { TokenMark }
  nothing   { TokenNothing_ }
  turn      { TokenTurn }
  ';'       { TokenNextC }
  Empty     { TokenEmptyPat }
  Lambda    { TokenLambdaPat }
  Debris    { TokenDebrisPat }
  Asteroid  { TokenAsteroidPat }
  Boundary  { TokenBoundaryPat }
  '_'       { TokenUnderscorePat }
  left      { TokenLeft }
  right     { TokenRight }
  front     { TokenFront }
  func      { TokenFunc $$ }

%%


Program :: { [Rule] }
Program : Rule {[$1]}
        | Rule Program { $1 : $2 }


Rule :: { Rule } 
Rule : Func SymArrow CmdList '.'{ Rule $1 $3 }


Func :: { Func}
Func : func { Functype $1}

SymArrow :: {SymArrow}
SymArrow : '->' {ArrowType $1}


Cmd :: { Cmd }
Cmd : go            {GoCmd}
    | take          {TakeCmd}
    | mark          {MarkCmd}
    | nothing       {NothingCmd}
    | turn Dir      {TurnCmd $2}
    | case Dir of AltList end  {CaseOfCmd $2 $4}
    | func          {FuncCmd (Functype $1)}

  
CmdList :: {[Cmd]}
CmdList : Cmd {[$1]}
        | Cmd ',' CmdList { $1 : $3 }


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
    | '_'       {UnderscorePat}




AltList :: {[Alt]}
AltList : Alt {[$1]}
        | Alt ';' AltList { $1 : $3 }

Alt :: {Alt}
Alt : Pat SymArrow CmdList { Alt $1 $3 }





{

happyError _ = error "parse error"

}