module Backend.OpenCL.Kernel where

import Data.Graph.Inductive
import Core.CoreGraph
import Backend.OpenCL.Tools
import Backend.OpenCL.Literal
import Backend.OpenCL.Function
import Backend.OpenCL.Param
import Backend.OpenCL.If

genKernel :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String
genKernel gr n@(i, ce) = case (ce, context gr i) of
        (CELit _, _) -> literalEntity gr n name
        (CEVar _, _) -> if isParamLN gr n then paramEntity gr n name else funcEntity gr n name
        (CEExpr _, _) -> if isParamLN gr n then paramEntity gr n name else funcEntity gr n name
        (CEPM v _, _) -> show v
        (CEIf _, _) -> ifEntity gr name n
        (_, _) -> ""
        where name = calcEntityName gr n

genKernels :: Gr CalcEntity EdgeRole -> (String, String)
genKernels gr = ("kernels.cl", concatMap (genKernel gr) $ labNodes gr)