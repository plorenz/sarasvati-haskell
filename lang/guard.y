{

--    This file is part of Sarasvati.
--
--    Sarasvati is free software: you can redistribute it and/or modify
--    it under the terms of the GNU Lesser General Public License as
--    published by the Free Software Foundation, either version 3 of the
--    License, or (at your option) any later version.
--
--    Sarasvati is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU Lesser General Public License for more details.
--
--    You should have received a copy of the GNU Lesser General Public
--    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.
--
--    Copyright 2008 Paul Lorenz

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
    not       { TokenNOT       }
    symbol    { TokenSymbol $$ }
    accept    { TokenAccept    }
    discard   { TokenDiscard   }
    skip      { TokenSkip      }
    '('       { TokenLP        }
    ')'       { TokenRP        }

%%

Stmt : if Expr then Stmt else Stmt { StmtIF $2 $4 $6   }
     | Result                      { StmtResult $1     }

Expr : UnitExpr or Expr  { ExprOR  $1 $3 }
     | UnitExpr and Expr { ExprAND $1 $3 }
     | not UnitExpr      { ExprNOT $2    }
     | UnitExpr          { $1            }

UnitExpr : symbol       { ExprSymbol $1 }
         | '(' Expr ')' { $2            }

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

data Expr = ExprOR  Expr Expr
          | ExprAND Expr Expr
          | ExprSymbol String
          | ExprNOT Expr
  deriving Show

data Token = TokenIF
           | TokenTHEN
           | TokenELSE
           | TokenAND
           | TokenOR
           | TokenNOT
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
                 ("NOT",cs)     -> TokenNOT         : lexer cs
                 ("Accept",cs)  -> TokenAccept      : lexer cs
                 ("Discard",cs) -> TokenDiscard     : lexer cs
                 ("Skip",cs)    -> TokenSkip        : lexer cs
                 (name,cs)      -> TokenSymbol name : lexer cs

}
