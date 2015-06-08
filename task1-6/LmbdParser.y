{
module LmbdParser where
import Data.Char
import Prelude
import Exp
}

%name lmbdParser
%tokentype { Token }
%error { parseError }

%token
    var {TokenVar $$}
    '\\' {TokenLmbd}
    '.' {TokenDot}
    '(' {TokenOB}
    ')' {TokenCB}
    'o' {TokenApp}


%% 

Exp : App {$1}

App : App 'o' Atom {App $1 $3}
    | Atom {$1}

Atom: '(' Exp ')' {$2}
    | var {Var $1}
    | '\\' var '.' '(' Exp ')' {Lmbd $2 $5}

{

parseError :: [Token] -> a
parseError t = error ("Parse error: " ++ (show t))

data Token = 
        TokenVar String
      | TokenLmbd
      | TokenDot
      | TokenOB
      | TokenCB
      | TokenApp
            deriving Show

isAlphaOrNumber c = or ((c == '\'') : (isAlpha c) : (isNumber c) : [])

lexer :: String -> [Token]
lexer [] = []
lexer ['\n'] = []
lexer (c:cs) 
      | isSpace c = TokenApp : lexer cs
      | isAlphaOrNumber c = lexAlpha (c:cs)
lexer ('\\':cs) = TokenLmbd : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexAlpha cs = TokenVar var : lexer rest
      where (var,rest) = span isAlphaOrNumber cs

normalize :: [Token] -> [Token]
normalize x = normalize' ([TokenOB] ++ x ++ [TokenCB]) [0]

normalize' x ((-1):(b:k)) = [TokenCB] ++ (normalize' x ((b-1):k))
normalize' [] _ = []
normalize' (TokenOB : x) (a:k) = [TokenOB] ++ (normalize' x ((a+1):k))
normalize' (TokenCB : x) (b:k) = [TokenCB] ++ (normalize' x ((b-1):k))
normalize' (TokenDot : x) k = [TokenDot] ++ [TokenOB] ++ (normalize' x (0:k))
normalize' (t : x) k = [t] ++ (normalize' x k)

glueGaps [] = []
glueGaps (' ':' ':s) = glueGaps (' ':s)
glueGaps (x:s) = x:(glueGaps s)

removeBadGaps [] = []
removeBadGaps ('\\':' ':s) = removeBadGaps ('\\':s)
removeBadGaps ('.':' ':s) = removeBadGaps ('.':s)
removeBadGaps (' ':'.':s) = removeBadGaps ('.':s)
removeBadGaps (')':' ':')':s) = "))" ++ (removeBadGaps s)
removeBadGaps ('(':' ':'(':s) = "((" ++ (removeBadGaps s)
removeBadGaps (x:s) = x:(removeBadGaps s)

parseLambda = lmbdParser . normalize . lexer . removeBadGaps . glueGaps

}



