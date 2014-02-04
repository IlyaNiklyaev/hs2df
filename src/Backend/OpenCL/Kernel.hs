module Backend.OpenCL.Kernel where

import Data.Graph.Inductive
import Core.CoreGraph
import Backend.OpenCL.Tools

--genKernel :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> (String, String)
--genKernel gr n@(i, ce) = case (ce, context gr i) of
--        (CELit l, _) -> (name, literalEntity gr n name)
--        (CEVar v, _) -> if isParamLN gr n then (name, paramEntity gr n name) else (name, funcEntity gr n name)
--        (CEExpr v, _) -> if isParamLN gr n then (name, paramEntity gr n name) else (name, funcEntity gr n name)
--        (CEPM v p, _) -> (name, show v)
--        (CEIf _, _) -> (name, ifEntity gr name n)
--        (_, _) -> (name, "")
--        where name = calcEntityName gr n

genKernels :: Gr CalcEntity EdgeRole -> (String, String)
genKernels gr = ("kernels.cl", "")