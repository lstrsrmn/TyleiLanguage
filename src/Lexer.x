{
module Lexer where

import Syntax
       }

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]
$capital = [A-Z]
$stringChars = $printable # \"
tokens :-

       "--".*                 ;
       $white+                ;
       \'\\n\'                {identifier (\s -> TokTermChar (((\_ -> '\n') . head . tail) <$> s))}
       \'$printable\'         {identifier (\s -> TokTermChar ((head . tail) <$> s))}
       \"$stringChars*\"      {identifier (\s -> TokString  ((init . tail) <$> s))}
       do                     {tok TokDo}
       Nat                    {tok TokNat}
       Char                   {tok TokChar}
       =                      {tok TokEquals}
       \+                     {tok TokPlus}
       \-                     {tok TokMinus}
       \*                     {tok TokTimes}
       \:\:                   {tok TokDoubleColon}
       \(\)                   {tok TokUnit}
       "->"                   {tok TokRightArrow}
       "<-"                   {tok TokLeftArrow}
       Type                   {tok TokType}
       return                 {tok TokReturn}
       \,                     {tok TokProduct}
       forall                 {tok TokForAll}
       ind                    {tok TokInd}
       \\                     {tok TokAbs}
       \.                     {tok TokDot}
       \|                     {tok TokCase}
       \_                     {tok TokWildcard}
       fst                    {tok TokFst}
       snd                    {tok TokSnd}
       \/\\                   {tok TokTypeAbs}
       \@                     {tok TokAt}
       fix                    {tok TokFix}
       let                    {tok TokLet}
       type                   {tok TokLetType}
       in                     {tok TokIn}
       matchChar              {tok TokMatchChar}
       with                   {tok TokWith}
       iter                   {tok TokIter}
       IO                     {tok TokIO}
       print                  {tok TokPrint}
       readFile               {tok TokReadFile}
       [A-Z] [$alphanum]*     {identifier TokCons}
       $alpha [$alphanum]*    {identifier TokVar}
       $digit+                {identifier (TokInt . (read <$>))}
       \;                     {tok TokSemiColon}
       \[                     {tok TokOpenSquareBracket}
       \]                     {tok TokCloseSquareBracket}
       \(                     {tok TokOpenBracket}
       \)                     {tok TokCloseBracket}
       \{                     {tok TokOpenCurlyBracket}
       \}                     {tok TokCloseCurlyBracket}


{

alexEOF :: Alex (Loc Token)
alexEOF = pure (L (SL (0,0) (0,0)) TokEOF)

tok :: Token -> AlexInput -> Int -> Alex (Loc Token)
tok t ((AlexPn _ line col), _, _, _) ln = pure (L (SL (line, col) (line, col + ln)) t)

identifier :: (Loc String -> Token) -> AlexInput -> Int -> Alex (Loc Token)
identifier t ((AlexPn _ line col), _, _, inp) ln =
           let
              sl = SL (line, col) (line, col + ln)
           in pure (L sl (t (L sl (take ln inp))))

data Token
     = TokOpenSquareBracket
     | TokCloseSquareBracket
     | TokOpenBracket
     | TokCloseBracket
     | TokOpenCurlyBracket
     | TokCloseCurlyBracket
     | TokSemiColon
     | TokString (Loc String)
     | TokPrint
     | TokReadFile
     | TokDo
     | TokNat
     | TokChar
     | TokTermChar (Loc Char)
     | TokUnit
     | TokDoubleColon
     | TokVar (Loc String)
     | TokCons (Loc String)
     | TokLeftArrow
     | TokRightArrow
     | TokReturn
     | TokInt (Loc Int)
     | TokProduct
     | TokForAll
     | TokInd
     | TokSingleQuote
     | TokAbs
     | TokDot
     | TokCase
     | TokWildcard
     | TokFst
     | TokSnd
     | TokTypeAbs
     | TokAt
     | TokLet
     | TokLetType
     | TokIn
     | TokFix
     | TokMatchChar
     | TokWith
     | TokIter
     | TokIO
     | TokEquals
     | TokPlus
     | TokMinus
     | TokTimes
     | TokType
     | TokEOF
     deriving Show
}
