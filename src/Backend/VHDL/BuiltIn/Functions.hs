module Backend.VHDL.BuiltIn.Functions where

import Var
import Core.CoreTools

getFuncBody :: Var -> String
getFuncBody v = case getVarName v of
        "+" -> "data <= d0 + d1;"
        "-" -> "data <= d0 - d1;"
        "*" -> "data <= d0 * d1;"
        "/" -> "data <= d0 * d1;"
        "I#" -> "data <= d0;"
        _ -> ""