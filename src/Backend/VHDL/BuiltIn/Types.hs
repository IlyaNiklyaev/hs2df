module Backend.VHDL.BuiltIn.Types where

import Type
import Outputable (showSDoc)
import Backend.VHDL.Types

getTypeIface' :: Type -> TypeIface
getTypeIface' t = case (showSDoc $ pprType t) of
        "GHC.Prim.Int#" -> TypeIface {sHigh = 15, sLow = 0, sType = "signed"}
        "GHC.Types.Int" -> TypeIface {sHigh = 15, sLow = 0, sType = "signed"}
        --"GHC.Num.Integer" -> TypeIface {sArity = 15, sType = "signed"}
        "GHC.Prim.Char#" -> TypeIface {sHigh = 15, sLow = 0, sType = "unsigned"}
        "GHC.Types.Char" -> TypeIface {sHigh = 15, sLow = 0, sType = "unsigned"}
        "GHC.Types.Bool" -> TypeIface {sHigh = 0, sLow = 0, sType = "unsigned"}
        "GHC.Prim.Float#" -> TypeIface {sHigh = 8, sLow = -23, sType = "float"}
        "GHC.Types.Float" -> TypeIface {sHigh = 8, sLow = -23, sType = "float"}
        "GHC.Prim.Double#" -> TypeIface {sHigh = 8, sLow = -23, sType = "float"}
        "GHC.Types.Double" -> TypeIface {sHigh = 8, sLow = -23, sType = "float"}
        _ -> TypeIface {sHigh = 0, sLow = 0, sType = showSDoc $ pprType t}

getTypeIface :: Type -> ([TypeIface], TypeIface)
getTypeIface t = (map getTypeIface' $ filter (not.isDictLikeTy) args, getTypeIface' res) where (args, res) = splitFunTys t