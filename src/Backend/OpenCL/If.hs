module Backend.OpenCL.If where

import Data.Graph.Inductive
import Core.CoreGraph
import Backend.OpenCL.Tools
import Data.List (intercalate)

ifEntity :: Gr CalcEntity EdgeRole -> String -> LNode CalcEntity -> String
ifEntity gr name n = unlines [
        "__kernel void " ++ name ++ "(" ++ (intercalate ", " $ map (\ (n, t) -> "__global *" ++ t ++ " " ++ n) $ calcEntityPort gr n) ++ ")",
        "{",
        concat [
                foldl (\ s i -> s ++ " else if (d0 == d" ++ show i ++ ") {\n         data = d" ++ show (i + aCount) ++ ";\n     }") ("    if(d0 == d1) {\n        data = d" ++ show (aCount + 1) ++ ";\n}") $ take (aCount - 1) [2,3..],
                if hasDefault then " else {\n        data = d" ++ show (aCount * 2 + 1) ++ ";\n }" else ""
        ],
        "}"
        ] where
                hasDefault = ((length $ calcEntityPort gr n) `mod` 2) == 1
                aCount = altCount gr n