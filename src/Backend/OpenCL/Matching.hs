module Backend.OpenCL.Matching where

import Backend.OpenCL.Tools
import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph
import Core.CoreTypes
import Graph.Tools
import Graph.Types

getMatchBody :: Int -> TypePort -> String
getMatchBody i ((inT:_),resT) = intercalate "\n" $ map (\ (ic,oc) -> "\t*" ++ getChannelName oc ++ " = *" ++ getChannelName ic ++ ";") $ zip (iChls !! index) oChls
                where
                        iChls = primitivizeType' "d0" inT
                        oChls = primitivizeType "data" resT
                        index = if length iChls > 1 then i + 1 else i

pmatchEntity :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
pmatchEntity gr ln@(_, ce) name = unlines [
        "__kernel void " ++ name ++ "(" ++ (intercalate ", " $ map (\ (n, t) -> "__global " ++ t ++ " *" ++ n) $ calcEntityPort gr ln) ++ ")",
        "{",
        "       " ++ (getMatchBody i $ calcEntityTypePort gr ln),
        "}"
        ] where i = case ce of
                        CEPM _ ind -> ind