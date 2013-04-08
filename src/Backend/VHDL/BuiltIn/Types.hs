module Backend.VHDL.BuiltIn.Types where

import Type
import Outputable (showSDoc)
import Backend.VHDL.Types

getTypeIface' :: Type -> TypeIface
getTypeIface' t = case (showSDoc $ pprType t) of
        "GHC.Prim.Int#" -> TypeIface {sArity = 32, sType = "signed"}
        "GHC.Types.Int" -> TypeIface {sArity = 32, sType = "signed"}
        "GHC.Num.Integer" -> TypeIface {sArity = 32, sType = "signed"}
        "GHC.Prim.Char#" -> TypeIface {sArity = 32, sType = "unsigned"}
        "GHC.Types.Char" -> TypeIface {sArity = 32, sType = "unsigned"}
        _ -> TypeIface {sArity = 0, sType = showSDoc $ pprType t}

getTypeIface :: Type -> ([TypeIface], TypeIface)
getTypeIface t = (map getTypeIface' $ filter (not.isDictLikeTy) args, getTypeIface' res) where (args, res) = splitFunTys t