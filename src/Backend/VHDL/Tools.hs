module Backend.VHDL.Tools where

import Data.Graph.Inductive
import Type
import Literal
import Var
import Data.Maybe (fromJust)
import Core.CoreGraph
import Backend.Common.Types
import Backend.Common.Tools
import Backend.VHDL.Types
import Backend.VHDL.BuiltIn.Types
import Backend.VHDL.BuiltIn.Functions
import Core.CoreTools
import Data.List (sortBy, elemIndex)

calcEntityTypeIface :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> TypeIface
calcEntityTypeIface gr = getTypeIface'.calcEntityType gr

emptyTypeIface :: TypeIface
emptyTypeIface = TypeIface {sHigh = 0, sType = ""}

fromTypePort :: TypePort -> Port
fromTypePort (iTypes, oType) = [
        ("first", "in",  "std_logic"),
        ("nex", "in",  "std_logic"),
        ("data", "out",  oBusType ++ " (" ++  show oBusHigh ++ " downto " ++ show oBusLow ++ ")"),
        ("ack", "out",  "std_logic")
        ] ++ (concatMap (\ (t, i) -> [
                ("f" ++ show i, "out","std_logic"),
                ("n" ++ show i, "out","std_logic"),
                ("d" ++ show i, "in",sType t ++ " (" ++  show (sHigh t) ++ " downto " ++  show (sLow t) ++ ")"),
                ("a" ++ show i, "in","std_logic")
                ]) $ zip iType [0,1..]) where
                oBusType = (sType.getTypeIface') oType
                oBusHigh = (sHigh.getTypeIface') oType
                oBusLow = (sLow.getTypeIface') oType
                iType = map (getTypeIface') iTypes

splitParamPort :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Port -> Port
splitParamPort gr ln@(n, _) port = (concatMap (\ i -> map (\ (fr, tp, to) -> (fr ++ show i, tp, to)) $ take 4 port) [0..(length $ out gr n) - 1]) ++ (drop 4 port)

calcEntityPort :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> Port
calcEntityPort gr n = (if isParamLN gr n then splitParamPort gr n else id) $ fromTypePort $ calcEntityTypePort gr n

getEdgePortMap :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> PortMap
getEdgePortMap gr e@(i, j, role) = ((i, cei), (j, cej), case (cej, role) of
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
                oPort = map (\ (x, _, y) -> (x, y)) $ take 4  $ drop (4 * (fromJust $ elemIndex e $ out gr i)) (calcEntityPort gr (i, cei))
                argPort arg = map (\ (x, _, _) -> x) $ take 4 $ drop (4 * (arg + 1)) (calcEntityPort gr (j, cej))
                cond = map (\ (x, _, _) -> x) $ take 4 $ drop 4 (calcEntityPort gr (j, cej))
                def = map (\ (x, _, _) -> x) $ reverse $ take 4 $ reverse (calcEntityPort gr (j, cej))
                altHead num = map (\ (x, _, _) -> x) $ take 4 $ drop (4 * (num + 2)) (calcEntityPort gr (j, cej))
                alt num = map (\ (x, _, _) -> x) $ take 4 $ drop (4 * (num + altCount gr (j, cej) + 2)) (calcEntityPort gr (j, cej))