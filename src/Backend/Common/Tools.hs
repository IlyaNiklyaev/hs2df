module Backend.Common.Tools where

import Data.Graph.Inductive
import Data.Maybe (fromJust)
import Core.CoreGraph
import Graph.Tools
import Backend.Common.Types
import Core.CoreTypes


primitivize :: TypePort -> TypePortPrimitive
primitivize (i,o) = (map (\ (num,t) -> primitivizeType ("d" ++ show num) t) $ zip [0,1..] i,primitivizeType "data" o)

getEdgePortMap' :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> PortMap'
getEdgePortMap' gr (i, j, role) = ((i, cei), (j, cej), case (cej, role) of
        (CELit _, _) -> []
        (_, Arg arg) -> zip iPort $ oPort ("d" ++ show arg)
        (_, Cond) -> zip iPort $ oPort "d0"
        (_, AltHead num) -> zip iPort $ oPort $ "d" ++ show (num + 1)
        (_, Alt num) -> zip iPort $ oPort $ "d" ++ show (num + 1)
        (_, Default) -> zip iPort $ oPort $ "d" ++ show defAlt
        (CEVar _, _) -> []
        (CEExpr _, _) -> []
        (CEPM _ _, _) -> []
        (_, _) -> []
        ) where
                cei = fromJust $ lab gr i
                cej = fromJust $ lab gr j
                (_,oPort') = (calcEntityTypePort gr (i, cei))
                oPort prefix = primitivizeType prefix oPort'
                iPort = oPort "data"
                defAlt = (length $ inn gr j) - 1