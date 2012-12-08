module Backend.OpenCL.Types where

type Port = [(String, String)]

data TypeIface = TypeIface {sType :: String}