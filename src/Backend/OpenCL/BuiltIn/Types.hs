module Backend.OpenCL.BuiltIn.Types where

import Type
import Outputable (showSDoc)
import Backend.OpenCL.Types

getTypeIface' :: Type -> TypeIface
getTypeIface' t = case (showSDoc $ pprType t) of
        "GHC.Prim.Int#" -> TypeIface {sType = "int"}
        "GHC.Types.Int" -> TypeIface {sType = "int"}
        --"GHC.Num.Integer" -> TypeIface {sType = "int"}
        "GHC.Prim.Char#" -> TypeIface {sType = "char"}
        "GHC.Types.Char" -> TypeIface {sType = "char"}
        "GHC.Types.Bool" -> TypeIface {sType = "bool"}
        "GHC.Prim.Float#" -> TypeIface {sType = "float"}
        "GHC.Types.Float" -> TypeIface {sType = "float"}
        "GHC.Prim.Double#" -> TypeIface {sType = "double"}
        "GHC.Types.Double" -> TypeIface {sType = "double"}
        _ -> TypeIface {sType = showSDoc $ pprType t}

getTypeIface :: Type -> ([TypeIface], TypeIface)
getTypeIface t = (map getTypeIface' $ filter (not.isDictLikeTy) args, getTypeIface' res) where (args, res) = splitFunTys t