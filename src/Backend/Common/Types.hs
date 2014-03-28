module Backend.Common.Types where

import Type
import Data.Graph.Inductive
import Core.CoreGraph
import Core.CoreTypes

type TypePort = ([Type], Type)

type TypePortPrimitive = ([[Channel]], [Channel])

type PortMap = (LNode CalcEntity, LNode CalcEntity, [(String, String, String)])

type PortMap' = (LNode CalcEntity, LNode CalcEntity, [(Channel, Channel)])