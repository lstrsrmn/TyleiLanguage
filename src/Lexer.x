{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$capital = [A-Z]

tokens :-

       "--".*          ;
       $white+         ;
       =               {\s -> TokEquals}
       \:\:            {\s -> TokType}
       \(\)            {\s -> TokUnit}
       "->"            {\s -> TokFunction}
       \,              {\s -> TokProduct}
       forAll          {\s -> TokForAll}
       ind             {\s -> TokInd}
       \`              {\s -> TokSingleQuote}
       \\              {\s -> TokAbs}
       \.              {\s -> TokDot}
       \|              {\s -> TokCase}
       \_              {\s -> TokWildcard}
       fst             {\s -> TokFst}
       snd             {\s -> TokSnd}
       \/\\            {\s -> TokTypeAbs}
       \@              {\s -> TokAt}
       fix             {\s -> TokFix}
       let             {\s -> TokLet}
       type            {\s -> TokLetType}
       in              {\s -> TokIn}
       matchChar       {\s -> TokMatchChar}
       iter            {\s -> TokIter}
       [A-Z] [$alpha]+ {\s -> TokCons s}
       $alpha+         {\s -> TokVar s}
       $digit+         {\s -> TokInt (read s)}
       \[              {\s -> TokOpenSquareBracket}
       \]              {\s -> TokCloseSquareBracket}
       \(              {\s -> TokOpenBracket}
       \)              {\s -> TokCloseBracket}


{

data Tokens
     = TokOpenSquareBracket
     | TokCloseSquareBracket
     | TokOpenBracket
     | TokCloseBracket
     | TokUnit
     | TokVar String
     | TokCons String
     | TokFunction
     | TokInt Int
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
     | TokIter
     | TokEquals
     | TokType
     deriving (Eq, Show)
}
