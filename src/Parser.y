{
module Parser where
import Lexer
import Syntax
import Control.Monad.Reader
import Data.List
}

%name parser
%tokentype {Loc Token}
%error {parseError}
%monad {Parser}
%lexer {lexer} {L _ TokEOF}




%token
       char            {L _ (TokTermChar $$)}
       do              {L _ TokDo}
       Nat             {L _ TokNat}
       Char            {L _ TokChar}
       '='             {L _ TokEquals}
       '+'             {L _ TokPlus}
       '-'             {L _ TokMinus}
       '*'             {L _ TokTimes}
       '::'            {L _ TokType}
       '()'            {L _ TokUnit}
       '->'            {L _ TokRightArrow}
       '<-'            {L _ TokLeftArrow}
       return          {L _ TokReturn}
       ','             {L _ TokProduct}
       forAll          {L _ TokForAll}
       ind             {L _ TokInd}
       '\\'            {L _ TokAbs}
       '.'             {L _ TokDot}
       '|'             {L _ TokCase}
       '_'             {L _ TokWildcard}
       fst             {L _ TokFst}
       snd             {L _ TokSnd}
       '\/\\'          {L _ TokTypeAbs}
       '\@'            {L _ TokAt}
       fix             {L _ TokFix}
       let             {L _ TokLet}
       type            {L _ TokLetType}
       in              {L _ TokIn}
       matchChar       {L _ TokMatchChar}
       with            {L _ TokWith}
       iter            {L _ TokIter}
       Cons            {L _ (TokCons $$)}
       Var             {L _ (TokVar $$)}
       Int             {L _ (TokInt $$)}
       IO              {L _ TokIO}
       print           {L _ TokPrint}
       readFile        {L _ TokReadFile}
       String          {L _ (TokString $$)}
       ';'             {L _ TokSemiColon}
       '['             {L _ TokOpenSquareBracket}
       ']'             {L _ TokCloseSquareBracket}
       '('             {L _ TokOpenBracket}
       ')'             {L _ TokCloseBracket}
       '{'             {L _ TokOpenCurlyBracket}
       '}'             {L _ TokCloseCurlyBracket}


%left '+' '-'
%left '*' '/'
%right in
%right '->'


%%

exp :: {Loc Raw}
    : '\\' binder '.' exp                                   {rloc (RAbs (syntax $2) $4) $1 $>}
    | let binder '::' type_exp '=' exp in exp               {rloc (RLet (syntax $2) $4 $6 $8) $1 $>}
    | matchChar '['type_exp']' exp with cases               {rloc (RMatchChar $3 $5 $7) $1 $6}
    | fix binder '::' type_exp branches                     {rloc (RFix (syntax $2) $4 $5) $1 $4}
    | '\/\\' typeBinder '.' exp                             {rloc (RTypeAbs (syntax $2) $4) $1 $>}
    | do '{' binder '::' type_exp '<-' exp ';' exp'}'       {rloc (RBind (syntax $3) $5 $7 $9) $1 $>}
    | let type typeBinder '=' type_exp in exp               {rloc (RLetType (syntax $3) $5 $7) $1 $>}
    | term                                                  {$1}

term : term atom                                            {rloc (RApp $1 $2) $1 $>}
     | term '\@' type_atom                                  {rloc (RTypeApp $1 $3) $1 $>}
     | fst atom                                             {rloc (RFst $2) $1 $>}
     | snd atom                                             {rloc (RSnd $2) $1 $>}
     | Cons atom                                            {rloc (RCons (syntax $1) $2) $1 $>}
     | return atom                                          {rloc (RReturn $2) $1 $>}
     | iter '['type_exp']' '('exp','exp','exp')'            {rloc (RIter $3 $6 $8 $10) $1 $>}
     | term '*' term                                        {rloc (RTimes $1 $3) $1 $>}
     | term '+' term                                        {rloc (RAdd $1 $3) $1 $>}
     | term '-' term                                        {rloc (RMinus $1 $3) $1 $>}
     | print atom                                           {rloc (RPrint $2) $1 $>}
     | readFile String                                      {rloc (RReadFile (syntax $2)) $1 $>}
     | atom                                                 {$1}

atom : '()'                                                 {rloc RStar $1 $>}
     | Var                                                  {rloc (RVar (syntax $1)) $1 $>}
     | Int                                                  {rloc (RNum (syntax $1)) $1 $>}
     | char                                                 {rloc (RChar (syntax $1)) $1 $>}
     | '('exp',' exp ')'                                    {rloc (RPair $2 $4) $1 $>}
     | '('exp')'                                            {$2}

type_exp : ind typeBinder with constructors        {rloc (Ind (syntax $2) $4) $1 $3}
         | type_atom '->' type_exp                 {rloc (Function (syntax $1) (syntax $3)) $1 $>}
         | forAll typeBinder type_exp              {rloc (ForAll (syntax $2) (syntax $3)) $1 $>}
         | IO type_atom                            {rloc (IO (syntax $2)) $1 $>}
         | type_atom                               {$1}

type_atom : '('type_exp','type_exp')'              {rloc (Product (syntax $2) (syntax $4)) $1 $>}
          | '()'                                   {rloc Unit $1 $>}
          | Nat                                    {rloc Nat $1 $>}
          | Cons                                   {rloc (TVar (syntax $1)) $1 $>}
          | Char                                   {rloc CharT $1 $>}
          | '('type_exp')'                         {$2}



binder : '_' {rloc Nothing $1 $>}
       | Var {rloc (Just (syntax $1)) $1 $>}

typeBinder : '_' {rloc Nothing $1 $>}
           | Cons {rloc (Just (syntax $1)) $1 $>}

cases : {-# empty #-}                                       {[]}
      | '|' '_' '->' exp cases                              {(Nothing, $4) : $5}
      | '|' char '->' exp cases                             {(Just (syntax $2), $4) : $5}

branches : {-# empty #-}                                    {[]}
         | '|' Cons binder '->' exp branches                   {($2, (syntax $3), $5) : $6}

constructors : {-# empty #-}                                {[]}
             | '|' Cons type_exp constructors               {((syntax $2), (syntax $3)) : $4}

{


type Parser a = ReaderT String Alex a

type LineNumber = Int

decrLine :: String -> String
decrLine ('\n':cs) = cs
decrLine (c:cs) = decrLine cs

getLn :: String -> String
getLn ('\n':cs) = ['\n']
getLn (c:cs) = c:getLn cs
getLn [] = ['\n']


findError :: SourceLocation -> String -> Parser String
findError sl source =
          pure (search sl source)
          where search :: SourceLocation -> String -> String
                search sl@(SL (1, c1) (l2, c2)) cs =  grab sl cs
                search (SL (l1, c1) (l2, c2)) ('\n':cs) = search (SL (l1-1, c1) (l2-1, c2)) cs
                search (SL (l1, c1) (l2, c2)) (c:cs) = search (SL (l1-1, c1) (l2-1, c2)) (decrLine cs)
                search _ [] = "unknown"
                grab :: SourceLocation -> String -> String
                grab (SL (1, s) (1, e)) cs = getLn cs ++ replicate (s-1) ' ' ++ replicate (e-s) '^'



parseError :: Loc Token -> Parser a
parseError (L sl@(SL (l1, c1) (l2, c2)) token) = do
           str <- ask
           inp <- findError sl str
           ReaderT (\_ -> alexError ("Parser failed from line: "++ show (l1 - 1) ++ " col: " ++ show c1 ++
                                    " to line: " ++ show (l2 - 1) ++ " col: " ++ show c2 ++ "\n\n" ++ inp))

slDiff :: SourceLocation -> SourceLocation -> SourceLocation
slDiff (SL (s1, c1) _) (SL _ (s2, c2)) = SL (s1, c1) (s2, c2)

rloc :: a -> Loc b -> Loc c -> Loc a
rloc r (L start _) (L end _) = L (slDiff start end) r

lexer :: (Loc Token -> Parser a) -> Parser a
lexer c = do
      nextToken <- lift alexMonadScan
      c nextToken

}
