module Backend.VHDL.Tools where

import Data.Graph.Inductive
import Type
import Literal
import Var
import Data.Maybe (fromJust)
import Core.CoreGraph
import Backend.VHDL.Types
import Backend.VHDL.BuiltIn.Types
import Core.CoreTools

calcEntityName :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String
calcEntityName gr n@(i, ce) = case ce of
        (CELit l) -> "lit" ++ show i
        (CEVar v) -> if isParamLN gr n then "var" ++ (getVarName v) else "func" ++ show i
        (CEExpr v) -> if isParamLN gr n then "var" ++ (getVarName v) else "func" ++ show i
        (CEPM v p) -> "pmatch[" ++ show p ++ "]" ++ show i
        (CEIf _) -> "if" ++ show i
        _ -> "other" ++ show i

calcEntityTypeIface :: CalcEntity -> TypeIface
calcEntityTypeIface = getTypeIface'.calcEntityType

calcEntityType :: CalcEntity -> Type
calcEntityType = snd.calcEntityTypePort

emptyTypeIface :: TypeIface
emptyTypeIface = TypeIface {sArity = 0, sType = ""}

fromTypePort :: TypePort -> Port
fromTypePort (iTypes, oType) = [
        ("first", "in",  "std_logic"),
        ("nex", "in",  "std_logic"),
        ("data", "out",  oBusType ++ " (" ++  show (oBusWidth - 1) ++ " downto 0)"),
        ("ack", "out",  "std_logic")
        ] ++ (concatMap (\ (t, a, i) -> [
                ("f" ++ show i, "out","std_logic"),
                ("n" ++ show i, "out","std_logic"),
                ("d" ++ show i, "in",t ++ " (" ++  show (a - 1) ++ " downto 0)"),
                ("a" ++ show i, "in","std_logic")
                ]) $ zip3 iBusT iBusW [0,1..]) where
                oBusType = (sType.getTypeIface') oType
                oBusWidth = (sArity.getTypeIface') oType
                iBusT = map (sType.getTypeIface') iTypes
                iBusW = map (sArity.getTypeIface') iTypes

calcEntityTypePort :: CalcEntity -> TypePort
calcEntityTypePort ce = case ce of
        (CELit l) -> ([], literalType l)
        (CEVar v) -> ([], varType v)
        (CEExpr v) -> (filter (not.isDictLikeTy) args, res) where (args, res) = splitFunTys $ varType v
        (CEPM v p) -> (filter (not.isDictLikeTy) args, res) where (args, res) = splitFunTys $ varType v
        --(CEIf) -> ([], )
        --_ -> ([], )

calcEntityPort :: CalcEntity -> Port
calcEntityPort = fromTypePort.calcEntityTypePort

getEdgePortMap :: Gr CalcEntity EdgeRole -> LEdge EdgeRole -> PortMap
getEdgePortMap gr (i, j, role) = ((i, cei), (j, cej), case (cej, role) of
        (CELit _, _) -> []
        (CEVar _, Arg arg) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ argPort arg
        (CEVar _, _) -> []
        (CEExpr _, Arg arg) -> map (\ ((x,y),z) -> (x,z,y)) $ zip oPort $ argPort arg
        (CEExpr _, _) -> []
        (CEPM v p, _) -> []
        (CEIf t, _) -> []
        (_, _) -> []
        ) where
                cei = fromJust $ lab gr i
                cej = fromJust $ lab gr j
                oPort = map (\ (x, _, y) -> (x, y)) $ take 4 (calcEntityPort cei)
                argPort arg = map (\ (x, _, _) -> x) $ take 4 $ drop (4 * (arg + 1)) (calcEntityPort cej)