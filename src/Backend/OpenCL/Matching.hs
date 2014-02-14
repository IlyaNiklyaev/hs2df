module Backend.OpenCL.Matching where

import Backend.OpenCL.BuiltIn.Functions
import Backend.OpenCL.Tools
import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph

pmatchEntity :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
pmatchEntity gr ln@(_, ce) name = unlines [
        "__kernel void " ++ name ++ "(" ++ (intercalate ", " $ map (\ (n, t) -> "__global " ++ t ++ " *" ++ n) $ calcEntityPort gr ln) ++ ")",
        "{",
        "       " ++ getMatchBody var,
        "}"
        ] where var = case ce of
                        CEPM v _ -> v