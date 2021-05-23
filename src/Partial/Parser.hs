module Partial.Parser where 

data Imp = D | S | N 

data Record = Language {name :: String , head :: String, imp :: Imp, body :: String}
type Records = [Record]