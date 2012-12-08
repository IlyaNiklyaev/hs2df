module Options where

import System.Console.GetOpt

data Argument = Input String | Output String | Dump String | Backend String

options :: [OptDescr Argument]
options = [
                Option ['t'] ["dumpTree"] (NoArg $ Dump "tree") "Dump Core AST",
                Option ['b'] ["dumpBinds"] (NoArg $ Dump "binds") "Dump local functions",
                Option ['d'] ["dumpTypes"] (NoArg $ Dump "types") "Dump local types",
                Option ['i','I'] ["input"] (ReqArg Input "FILE") "Haskell source FILE",
                Option ['o','O'] ["output"] (ReqArg Output "DIR") "Output directory",
                Option ['v'] ["vhdl"] (NoArg $ Backend "vhdl") "Generate VHDL code",
                Option ['g'] ["graph"] (NoArg $ Backend "graph") "Generate .dot graph representation",
                Option ['c'] ["opencl"] (NoArg $ Backend "opencl") "Generate OpenCL program"
          ]

isValidOptions :: [Argument] -> Bool
isValidOptions opts = and [not $ null $ outDirectory opts, not $ null $ sourceFile opts, not $ null $ (activeBackends opts) ++ (activeDumps opts)]

outDirectory :: [Argument] -> String
outDirectory [] = ""
outDirectory (Output dir:_) = dir
outDirectory (_:args) = outDirectory args

sourceFile :: [Argument] -> String
sourceFile [] = ""
sourceFile (Input file:_) = file
sourceFile (_:args) = sourceFile args

activeBackends :: [Argument] -> [String]
activeBackends = foldl (\ res x -> case x of Backend str -> (str:res); _ -> res) []

activeDumps :: [Argument] -> [String]
activeDumps = foldl (\ res x -> case x of Dump str -> (str:res); _ -> res) []