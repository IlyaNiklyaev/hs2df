module Backend.Common.Types where

import Type
import Data.Graph.Inductive
import Core.CoreGraph

type TypePort = ([Type], Type)

type PortMap = (LNode CalcEntity, LNode CalcEntity, [(String, String, String)])