module Backend.OpenCL.Param where

import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph
import Backend.OpenCL.Tools

paramEntity ::  Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
paramEntity gr ln name = unlines [
        "__kernel void " ++ name ++ "(" ++ (intercalate ", " $ map (\ (n, t) -> "__global *" ++ t ++ " " ++ n) $ calcEntityPort gr ln) ++ ")",
        "{",
        "       *data = *d0;",
        "}"
        ]