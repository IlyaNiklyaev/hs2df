module Backend.VHDL.BuiltIn.Functions where

import Var
import Core.CoreTools
import Backend.VHDL.BuiltIn.Types
import Backend.VHDL.Types

getFuncBody :: Var -> String
getFuncBody v = case getVarName v of
        "+" -> "data <= d0 + d1;"
        "-" -> "data <= d0 - d1;"
        "*" -> "data <= resize(d0 * d1, " ++ if (sType.snd.getTypeIface.varType) v == "float" then "data'high, -data'low);" else " data'length);"
        "/" -> "data <= resize(d0 / d1, " ++ if (sType.snd.getTypeIface.varType) v == "float" then "data'high, -data'low);" else " data'length);"
        "I#" -> "data <= d0;"
        "F#" -> "data <= d0;"
        "D#" -> "data <= d0;"
        "True" -> "data <= \"1\";"
        "False" -> "data <= \"0\";"
        ">" -> "data <= \"1\" when d0 > d1 else \"0\";"
        ">=" -> "data <= \"1\" when d0 >= d1 else \"0\";"
        "<" -> "data <= \"1\" when d0 < d1 else \"0\";"
        "<=" -> "data <= \"1\" when d0 <= d1 else \"0\";"
        "==" -> "data <= \"1\" when d0 = d1 else \"0\";"
        "/=" -> "data <= \"1\" when d0 /= d1 else \"0\";"
        _ -> ""