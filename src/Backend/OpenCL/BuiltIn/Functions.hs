module Backend.OpenCL.BuiltIn.Functions where

import Var
import Core.CoreTools

getFuncBody :: Var -> String
getFuncBody v = case getVarName v of
        "+" -> "*data = *d0 + *d1;"
        "-" -> "*data = *d0 - *d1;"
        "*" -> "*data = *d0 * *d1;"
        "/" -> "*data = *d0 / *d1;"
        "negate" -> "*data = - *d0;"
        "abs" -> "*data = abs(*d0);"
        "I#" -> "*data = *d0;"
        "F#" -> "*data = *d0;"
        "D#" -> "*data = *d0;"
        "True" -> "*data = true;"
        "False" -> "*data = false;"
        ">" -> "*data = *d0 > *d1 ? true : false"
        ">=" -> "*data = *d0 >= *d1 ? true : false"
        "<" -> "*data = *d0 < *d1 ? true : false"
        "<=" -> "*data = *d0 <= *d1 ? true : false"
        "==" -> "*data = *d0 == *d1 ? true : false"
        "/=" -> "*data = *d0 != *d1 ? true : false"
        _ -> ""