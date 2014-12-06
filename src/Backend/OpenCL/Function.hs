module Backend.OpenCL.Function where

import Backend.Common.Types
import Backend.Common.Tools
import Backend.OpenCL.BuiltIn.Functions
import Backend.OpenCL.Tools
import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph
import Core.CoreTypes
import Graph.Tools
import DataCon
import Id (isDataConId_maybe)

getDataConBody :: DataCon -> TypePortPrimitive -> String
getDataConBody dc (iChls,oChls) = intercalate "\n" $ case head oChls of
                                        Tag tagName -> (("\t*" ++ tagName ++ " = " ++ show (dataConTag dc) ++ ";"): (map tpl $ zip (concat iChls) $ tail oChls))
                                        Typed _ _ -> map tpl $ zip (concat iChls) oChls
                                   where tpl (i,o) = "\t*" ++ getChannelName o ++ " = *" ++ getChannelName i ++ ";"

funcEntity :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
funcEntity gr ln@(_, ce) name = unlines [
        "__kernel void " ++ name ++ "(" ++ (intercalate ", " $ map (\ (n, t) -> "__global " ++ t ++ " *" ++ n) $ calcEntityPort gr ln) ++ ")",
        "{",
        "       " ++ case isDataConId_maybe var of Just dc -> getDataConBody dc $ primitivize $ calcEntityTypePort gr ln ; Nothing -> getFuncBody var,
        "}"
        ] where var = case ce of
                        CEVar v -> v
                        CEExpr v -> v