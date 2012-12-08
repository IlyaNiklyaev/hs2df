module Backend.VHDL.VHDL where

import Data.Graph.Inductive
import Core.CoreGraph
import Graph.Tools
import Control.Monad
import System.IO
import System.Directory
import Backend.Common.Tools
import Backend.VHDL.Literal
import Backend.VHDL.Function
import Backend.VHDL.If
import Backend.VHDL.TopEntity
import Backend.VHDL.Param

getVHDLFile :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> (String, String)
getVHDLFile gr n@(i, ce) = case (ce, context gr i) of
        (CELit _, _) -> (name, literalEntity gr n name)
        (CEVar _, _) -> if isParamLN gr n then (name, paramEntity gr n name) else (name, funcEntity gr n name)
        (CEExpr _, _) -> if isParamLN gr n then (name, paramEntity gr n name) else (name, funcEntity gr n name)
        (CEPM v _, _) -> (name, show v)
        (CEDMerge _, _) -> (name, ifEntity gr name n)
        (_, _) -> (name, "")
        where name = calcEntityName gr n

genTopEntity :: Gr CalcEntity EdgeRole -> (String, String)
genTopEntity gr = ("device", topEntity "device" gr)

genVHDL :: Gr CalcEntity EdgeRole -> String -> IO ()
genVHDL gr dir = do
        createDirectoryIfMissing True dir
        forM_ (genTopEntity gr : (map (getVHDLFile gr) $ labNodes gr)) (\ (name, content) -> do
        h <- openFile (dir ++ name ++ ".vhdl") WriteMode
        hPutStr h content
        hClose h)