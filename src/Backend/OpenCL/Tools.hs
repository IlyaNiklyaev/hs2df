module Backend.OpenCL.Tools where

import Data.Graph.Inductive
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

fromTypePort' :: TypePortPrimitive -> Port
fromTypePort' (iChls, oChls) = map (\ ch -> (getChannelName ch, getChannelType ch)) (concat iChls ++ oChls)

calcEntityPort :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Port
calcEntityPort gr n = fromTypePort'.primitivize $ calcEntityTypePort gr n

getChannelType :: Channel -> String
getChannelType (Tag _) = "int"
getChannelType (Typed _ t) = (sType.getTypeIface') t

getEdgePortMap :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> PortMap
getEdgePortMap gr e = (i,j,portMap) where (i,j,portMap') = getEdgePortMap' gr e; portMap = map (\ (ch1,ch2) -> (getChannelName ch1,getChannelName ch2,getChannelType ch1)) portMap'