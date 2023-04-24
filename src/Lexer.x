{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

       "--".*     ;
       $white+    ;
       let        {\s -> Let}
       letType    {\s -> LetType}
       in         {\s -> In}
       \-\>       {\s -> Function}
       \,         {\s -> Product}
       fix        {\s -> Fix}
       matchChar  {\s -> MatchChar}
       iter       {\s -> Iter}
       =          {\s -> Abs}
       fst        {\s -> Fst}
       \/\\       {\s -> TypeAbs}
       $alpha+    {\s -> Var s}


{

data Token
     = Let
     | LetType
     | In
     | Function
     | Product
     | Fix
     | MatchChar
     | Iter
     | Abs
     | Fst
     | TypeAbs
     | Int Int
     | Var String
     deriving (Eq, Show)

main = do
     s <- getContents
     print (alexScanTokens s)
}
