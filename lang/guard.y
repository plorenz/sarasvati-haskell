{
module Workflow.GuardLang where
import qualified Workflow.EngineTypes as EngineTypes
import Data.Char
}

%name evalGuard
%tokentype { Token }
%error { parseError }

%token
    if		    { TokenIF        }
    then      { TokenTHEN      }
    else      { TokenELSE      }
    and       { TokenAND       }
    or        { TokenOR        }
    symbol    { TokenSymbol $$ }
    accept    { TokenAccept    }
    discard   { TokenDiscard   }
    skip      { TokenSkip      }
    '('       { TokenLP        }
    ')'       { TokenRP        }

%%

Stmt : if Expr then Stmt else Stmt { StmtIF $2 $4 $6   }
     | Result                      { StmtResult $1     }

Expr : symbol or Expr  { ExprOR  $1 $3 }
     | symbol and Expr { ExprAND $1 $3 }
     | '(' Expr ')'    { $2            }
     | symbol          { ExprSymbol $1 }

Result : accept      { EngineTypes.AcceptToken  }
       | discard     { EngineTypes.DiscardToken }
       | skip symbol { EngineTypes.SkipNode $2  }
       | skip        { EngineTypes.SkipNode []  }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Stmt = StmtIF Expr Stmt Stmt
          | StmtResult EngineTypes.GuardResponse
  deriving Show

data Expr = ExprOR  String Expr
          | ExprAND String Expr
          | ExprSymbol String
  deriving Show

data Token = TokenIF
           | TokenTHEN
           | TokenELSE
           | TokenAND
           | TokenOR
           | TokenSymbol String
           | TokenAccept
           | TokenDiscard
           | TokenSkip
           | TokenLP
           | TokenRP
  deriving Show

isLegalSymbolChar '.' = True
isLegalSymbolChar '_' = True
isLegalSymbolChar x   = isAlpha x || isDigit x

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexWord (c:cs)
lexer ('(':cs) = TokenLP : lexer cs
lexer (')':cs) = TokenRP : lexer cs

lexWord :: String -> [Token]
lexWord cs = case span isLegalSymbolChar cs of
                 ("IF",cs)      -> TokenIF          : lexer cs
                 ("THEN",cs)    -> TokenTHEN        : lexer cs
                 ("ELSE",cs)    -> TokenELSE        : lexer cs
                 ("AND",cs)     -> TokenAND         : lexer cs
                 ("OR",cs)      -> TokenOR          : lexer cs
                 ("Accept",cs)  -> TokenAccept      : lexer cs
                 ("Discard",cs) -> TokenDiscard     : lexer cs
                 ("Skip",cs)    -> TokenSkip        : lexer cs
                 (name,cs)      -> TokenSymbol name : lexer cs

}

