module Backend.OpenCL.Function where

import Backend.OpenCL.BuiltIn.Functions
import Backend.OpenCL.Tools
import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph

funcEntity :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
funcEntity gr ln@(_, ce) name = unlines [
        "__kernel void " ++ name ++ "(" ++ (intercalate ", " $ map (\ (n, t) -> "__global " ++ t ++ " *" ++ n) $ calcEntityPort gr ln) ++ ")",
        "{",
        "       " ++ getFuncBody var,
        "}"
        ] where var = case ce of
                        CEVar v -> v
                        CEExpr v -> v