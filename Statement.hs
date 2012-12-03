module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
	Skip|
	Begin Statement|
    If Expr.T Statement Statement|
	While Expr.T Statement|
	Read String|
	Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip s = Skip

beginState = accept "begin" # parse #- require "end" >-> buildBegin
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
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
