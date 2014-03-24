module Backend.OpenCL.Tools where

import Data.Graph.Inductive
import Data.Maybe (fromJust)
import Core.CoreGraph
import Core.CoreTypes
import Backend.Common.Types
import Backend.Common.Tools
import Backend.OpenCL.Types
import Backend.OpenCL.BuiltIn.Types

calcEntityTypeIface :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> TypeIface
calcEntityTypeIface gr = getTypeIface'.calcEntityType gr

emptyTypeIface :: TypeIface
emptyTypeIface = TypeIface {sType = ""}

fromTypePort :: TypePort -> Port
fromTypePort (iTypes, oType) = ("data", oBusType) : (concatMap (\ (t, i) -> [
                ("d" ++ show i, sType t)
                ]) $ zip iType [0,1..]) where
                oBusType = (sType.getTypeIface') oType
                iType = map (getTypeIface') iTypes

calcEntityPort :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Port
calcEntityPort gr n = fromTypePort $ calcEntityTypePort gr n

getEdgePortMap2 :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> PortMap
getEdgePortMap2 gr (i, j, role) = ((i, cei), (j, cej), case (cej, role) of
        (CELit _, _) -> []
        (_, Arg arg) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ argPort arg
        (_, Cond) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ cond
        (_, AltHead num) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ altHead num
        (_, Alt num) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ alt num
        (_, Default) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ def
        (CEVar _, _) -> []
        (CEExpr _, _) -> []
        (CEPM v p, _) -> []
        (_, _) -> []
        ) where
                cei = fromJust $ lab gr i
                cej = fromJust $ lab gr j
                oPort = take 1 $ (calcEntityPort gr (i, cei))
                argPort arg = map fst $ take 1 $ drop (1 * (arg + 1)) (calcEntityPort gr (j, cej))
                cond = map fst $ take 1 $ drop 1 (calcEntityPort gr (j, cej))
                def = map fst $ reverse $ take 1 $ reverse (calcEntityPort gr (j, cej))
                altHead num = map fst $ take 1 $ drop (num + 2) (calcEntityPort gr (j, cej))
                alt num = map fst $ take 1 $ drop (num + altCount gr (j, cej) + 2) (calcEntityPort gr (j, cej))

getChannelType :: Channel -> String
getChannelType (Tag _) = "int"
getChannelType (Typed _ t) = (sType.getTypeIface') t

getEdgePortMap :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> PortMap
getEdgePortMap gr e = (i,j,portMap) where (i,j,portMap') = getEdgePortMap' gr e; portMap = map (\ (chId,ch) -> ("data","d" ++ show chId,getChannelType ch)) portMap'