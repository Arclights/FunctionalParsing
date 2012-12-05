module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T
	| Skip
	| Begin [Statement]
    | If Expr.T Statement Statement
	| While Expr.T Statement
	| Read String
	| Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip s = Skip

beginState = accept "begin" # iter parse #- require "end" >-> buildBegin
buildBegin (v, e) = Begin e

ifState = accept "if" # Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf (((s, v), e), g) = If v e g

whileState = accept "while" # Expr.parse #- require "do" # parse >-> buildWhile
buildWhile ((s, v), e) = While v e

readState = accept "read" # word #- require ";" >-> buildRead
buildRead (v, e) = Read e

writeState = accept "write" # Expr.parse #- require ";" >-> buildWrite
buildWrite (v, e) = Write e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment str expr :stmts) dict input = exec stmts (Dictionary.insert (str, (Expr.value expr dict)) dict) input
exec (Skip :stmts) dict input = []
exec (Begin stmts1 :stmts2) dict input = (exec stmts1 dict input)++(exec stmts2 dict input)
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond stmt :stmts) dict input = 
	if (Expr.value cond dict)>0
	then exec (stmt:(While cond stmt):stmts) dict input
	else exec stmts dict input
exec (Read str :stmts) dict (i:is) = exec stmts (Dictionary.insert (str, i) dict) is
exec (Write expr :stmts) dict input = (Expr.value expr dict):(exec stmts dict input)


instance Parse Statement where
  parse = skip ! beginState ! ifState ! whileState ! readState ! writeState ! assignment
  toString = error "Statement.toString not implemented"
