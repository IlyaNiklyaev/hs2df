module Backend.VHDL.Types where

import Type
import Data.Graph.Inductive
import Core.CoreGraph

type Port = [(String, String ,String)]

data TypeIface = TypeIface {sHigh :: Int, sLow :: Int, sType :: String}

type TypePort = ([Type], Type)

type TypePortMap = [(TypePort, TypePort)]

type PortMap = (LNode CalcEntity, LNode CalcEntity, [(String, String, String)])