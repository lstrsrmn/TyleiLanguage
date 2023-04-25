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
       \'$printable \' {\(_:s:_) -> TokTermChar s}
       do              {\s -> TokDo}
       Nat             {\s -> TokNat}
       Char            {\s -> TokChar}
       =               {\s -> TokEquals}
       \:\:            {\s -> TokType}
       \(\)            {\s -> TokUnit}
       "->"            {\s -> TokRightArrow}
       "<-"            {\s -> TokLeftArrow}
       return          {\s -> TokReturn}
       \,              {\s -> TokProduct}
       forAll          {\s -> TokForAll}
       ind             {\s -> TokInd}
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
       with            {\s -> TokWith}
       iter            {\s -> TokIter}
       IO              {\s -> TokIO}
       [A-Z] [$alpha]+ {\s -> TokCons s}
       $alpha+         {\s -> TokVar s}
       $digit+         {\s -> TokInt (read s)}
       \;              {\s -> TokSemiColon}
       \[              {\s -> TokOpenSquareBracket}
       \]              {\s -> TokCloseSquareBracket}
       \(              {\s -> TokOpenBracket}
       \)              {\s -> TokCloseBracket}
       \{              {\s -> TokOpenCurlyBracket}
       \}              {\s -> TokCloseCurlyBracket}


{

data Token
     = TokOpenSquareBracket
     | TokCloseSquareBracket
     | TokOpenBracket
     | TokCloseBracket
     | TokOpenCurlyBracket
     | TokCloseCurlyBracket
     | TokSemiColon
     | TokDo
     | TokNat
     | TokChar
     | TokTermChar Char
     | TokUnit
     | TokVar String
     | TokCons String
     | TokLeftArrow
     | TokRightArrow
     | TokReturn
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
     | TokWith
     | TokIter
     | TokIO
     | TokEquals
     | TokType
     deriving (Eq, Show)
}
