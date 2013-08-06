module Backend.OpenCL.Kernel where

import Data.Graph.Inductive
import Core.CoreGraph

genKernels :: Gr CalcEntity EdgeRole -> (String, String)
genKernels gr = ("kernels.cl", "")