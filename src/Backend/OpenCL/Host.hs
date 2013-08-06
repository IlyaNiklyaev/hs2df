module Backend.OpenCL.Host where

import Data.Graph.Inductive
import Core.CoreGraph

genHostBody :: Gr CalcEntity EdgeRole -> (String, String)
genHostBody gr = ("body.c", "")

genHostHeader :: Gr CalcEntity EdgeRole -> (String, String)
genHostHeader gr = ("header.h", "")