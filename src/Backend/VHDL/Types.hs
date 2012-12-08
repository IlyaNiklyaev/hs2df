module Backend.VHDL.Types where

type Port = [(String, String ,String)]

data TypeIface = TypeIface {sHigh :: Int, sLow :: Int, sType :: String}