{
module Parser where
import Lexer
import Syntax
}

%name parser
%tokentype {Token}
%error {parseError}




%token
       char            {TokTermChar $$}
       do              {TokDo}
       Nat             {TokNat}
       Char            {TokChar}
       '='             {TokEquals}
       '+'             {TokPlus}
       '-'             {TokMinus}
       '*'             {TokTimes}
       '::'            {TokType}
       '()'            {TokUnit}
       '->'            {TokRightArrow}
       '<-'            {TokLeftArrow}
       return          {TokReturn}
       ','             {TokProduct}
       forAll          {TokForAll}
       ind             {TokInd}
       '\\'            {TokAbs}
       '.'             {TokDot}
       '|'             {TokCase}
       '_'             {TokWildcard}
       fst             {TokFst}
       snd             {TokSnd}
       '\/\\'          {TokTypeAbs}
       '\@'            {TokAt}
       fix             {TokFix}
       let             {TokLet}
       type            {TokLetType}
       in              {TokIn}
       matchChar       {TokMatchChar}
       with            {TokWith}
       iter            {TokIter}
       Cons            {TokCons $$}
       Var             {TokVar $$}
       Int             {TokInt $$}
       IO              {TokIO}
       print           {TokPrint}
       readFile        {TokReadFile}
       String          {TokString $$}
       ';'             {TokSemiColon}
       '['             {TokOpenSquareBracket}
       ']'             {TokCloseSquareBracket}
       '('             {TokOpenBracket}
       ')'             {TokCloseBracket}
       '{'             {TokOpenCurlyBracket}
       '}'             {TokCloseCurlyBracket}


%%

exp : '\\' binder '.' exp                                   {RAbs $2 $4}
    | let binder '::' type_exp '=' exp in exp               {RLet $2 $4 $6 $8}
    | matchChar '['type_exp']' exp with cases               {RMatchChar $3 $5 $7}
    | fix binder '::' type_exp branches                     {RFix $2 $4 $5}
    | '\/\\' typeBinder '.' exp                             {RTypeAbs $2 $4}
    | do '{' binder '::' type_exp '<-' exp ';' exp'}'       {RBind $3 $5 $7 $9}
    | let type typeBinder '=' type_exp in exp               {RLetType $3 $5 $7}
    | term '*' exp                                          {RTimes $1 $3}
    | term                                                  {$1}

term : term atom                                            {RApp $1 $2}
     | term '\@' type_atom                                  {RTypeApp $1 $3}
     | fst atom                                             {RFst $2}
     | snd atom                                             {RSnd $2}
     | Cons atom                                            {RCons $1 $2}
     | return atom                                          {RReturn $2}
     | iter '['type_exp']' '('exp','exp','exp')'            {RIter $3 $6 $8 $10}
     | atom '+' term                                        {RAdd $1 $3}
     | atom '-' term                                        {RMinus $1 $3}
     | print atom                                           {RPrint $2}
     | readFile String                                      {RReadFile $2}
     | atom                                                 {$1}

atom : '()'                                                 {RStar}
     | Var                                                  {RVar $1}
     | Int                                                  {RNum $1}
     | char                                                 {RChar $1}
     | '('exp')'                                            {$2}
     | '('exp',' exp ')'                                    {RPair $2 $4}


type_exp : ind typeBinder with constructors        {Ind $2 $4}
         | type_atom '->' type_exp                 {Function $1 $3}
         | forAll typeBinder type_exp              {ForAll $2 $3}
         | IO type_atom                            {IO $2}
         | type_atom                               {$1}

type_atom : '('type_exp','type_exp')'              {Product $2 $4}
          | '()'                                   {Unit}
          | Nat                                    {Nat}
          | Cons                                   {TVar $1}
          | Char                                   {CharT}
          | '('type_exp')'                         {$2}



binder : '_' {Nothing}
       | Var {Just $1}

typeBinder : '_' {Nothing}
           | Cons {Just $1}

cases : {-# empty #-}                                       {[]}
      | '|' '_' '->' exp cases                              {(Nothing, $4) : $5}
      | '|' char '->' exp cases                             {(Just $2, $4) : $5}

branches : {-# empty #-}                                    {[]}
         | '|' Cons Var '->' exp branches                   {($2, $3, $5) : $6}

constructors : {-# empty #-}                                {[]}
             | '|' Cons type_exp constructors               {($2, $3) : $4}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
