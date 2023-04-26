{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$capital = [A-Z]
$stringChars = $printable # \"

tokens :-

       "--".*          ;
       $white+         ;
       \'\\n\'         {\_ -> TokTermChar '\n'}
       \'$printable\'  {\(_:s:_) -> TokTermChar s}
       \"$stringChars*\"{TokString . init . tail}
       do              {\_ -> TokDo}
       Nat             {\_ -> TokNat}
       Char            {\_ -> TokChar}
       =               {\_ -> TokEquals}
       \+              {\_ -> TokPlus}
       \-              {\_ -> TokMinus}
       \*              {\_ -> TokTimes}
       \:\:            {\_ -> TokType}
       \(\)            {\_ -> TokUnit}
       "->"            {\_ -> TokRightArrow}
       "<-"            {\_ -> TokLeftArrow}
       return          {\_ -> TokReturn}
       \,              {\_ -> TokProduct}
       forAll          {\_ -> TokForAll}
       ind             {\_ -> TokInd}
       \\              {\_ -> TokAbs}
       \.              {\_ -> TokDot}
       \|              {\_ -> TokCase}
       \_              {\_ -> TokWildcard}
       fst             {\_ -> TokFst}
       snd             {\_ -> TokSnd}
       \/\\            {\_ -> TokTypeAbs}
       \@              {\_ -> TokAt}
       fix             {\_ -> TokFix}
       let             {\_ -> TokLet}
       type            {\_ -> TokLetType}
       in              {\_ -> TokIn}
       matchChar       {\_ -> TokMatchChar}
       with            {\_ -> TokWith}
       iter            {\_ -> TokIter}
       IO              {\_ -> TokIO}
       print           {const TokPrint}
       readFile        {const TokReadFile}
       [A-Z] [$alpha]* {\s -> TokCons s}
       $alpha+         {\s -> TokVar s}
       $digit+         {\s -> TokInt (read s)}
       \;              {\_ -> TokSemiColon}
       \[              {\_ -> TokOpenSquareBracket}
       \]              {\_ -> TokCloseSquareBracket}
       \(              {\_ -> TokOpenBracket}
       \)              {\_ -> TokCloseBracket}
       \{              {\_ -> TokOpenCurlyBracket}
       \}              {\_ -> TokCloseCurlyBracket}


{

data Token
     = TokOpenSquareBracket
     | TokCloseSquareBracket
     | TokOpenBracket
     | TokCloseBracket
     | TokOpenCurlyBracket
     | TokCloseCurlyBracket
     | TokSemiColon
     | TokString String
     | TokPrint
     | TokReadFile
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
     | TokPlus
     | TokMinus
     | TokTimes
     | TokType
     deriving (Eq, Show)
}
