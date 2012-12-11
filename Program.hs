module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined
instance Parse T where
  parse = iter (Statement.parse) >-> Program
  toString (Program stmts) = concat (map (Statement.toString) stmts)

exec (Program stmts) = Statement.exec stmts Dictionary.empty
