module Backend.OpenCL.Matching where

import Backend.Common.Types
import Backend.Common.Tools
import Backend.OpenCL.Tools
import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph
import Core.CoreTypes

getMatchBody :: Int -> TypePortPrimitive -> String
getMatchBody i (iChls,oChls) = intercalate "\n" $ map (\ (ic,oc) -> "\t*" ++ getChannelName oc ++ " = *" ++ getChannelName ic ++ ";") $ zip (iChls !! i) oChls

pmatchEntity :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
pmatchEntity gr ln@(_, ce) name = unlines [
        "__kernel void " ++ name ++ "(" ++ (intercalate ", " $ map (\ (n, t) -> "__global " ++ t ++ " *" ++ n) $ calcEntityPort gr ln) ++ ")",
        "{",
        "       " ++ (getMatchBody i $ primitivize $ calcEntityTypePort gr ln),
        "}"
        ] where i = case ce of
                        CEPM _ ind -> ind