module Backend.OpenCL.Literal where

import Literal
import FastString (unpackFS)
import Numeric (showIntAtBase, fromRat)
import Data.Char (intToDigit)
import Text.Printf
import Backend.OpenCL.BuiltIn.Types
import Backend.OpenCL.Types
import Data.Graph.Inductive
import Core.CoreGraph

literalBinaryValue :: Literal -> String
literalBinaryValue (MachChar val) = show val
literalBinaryValue (MachInt val) = show val
literalBinaryValue (MachWord val) = show val
literalBinaryValue (MachInt64 val) = show val
literalBinaryValue (MachFloat val) = show $ fromRat val
literalBinaryValue (MachDouble val) = show $ fromRat val
literalBinaryValue (MachStr val) = printf "%b" $ unpackFS val
literalBinaryValue (LitInteger val _) = showIntAtBase 2 intToDigit val ""
literalBinaryValue (MachLabel val _ _) = printf "%b" $ unpackFS val

literalEntity :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
literalEntity _ (_, CELit lit) name = unlines [
        "__kernel void " ++ name ++ "(__global " ++ busType ++ " *data)",
        "{",
        "       *data = " ++ (literalBinaryValue lit) ++ ";",
        "}"
        ] where
                busType = (sType.snd.getTypeIface.literalType) lit