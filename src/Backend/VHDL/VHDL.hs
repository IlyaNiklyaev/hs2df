module Backend.VHDL.VHDL where

import Data.Graph.Inductive
import Core.CoreGraph
import Control.Monad
import System.IO
import System.Directory
import Backend.VHDL.Literal
import Backend.VHDL.Function
import Backend.VHDL.If
import Backend.VHDL.TopEntity
import Backend.VHDL.Param
import Backend.VHDL.Types
import Backend.VHDL.Tools

getVHDLFile :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> (String, String)
getVHDLFile gr n@(i, ce) = case (ce, context gr i) of
        (CELit l, _) -> (name, literalEntity gr n name)
        (CEVar v, _) -> if isParamLN gr n then (name, paramEntity gr n name) else (name, funcEntity gr n name)
        (CEExpr v, _) -> if isParamLN gr n then (name, paramEntity gr n name) else (name, funcEntity gr n name)
        (CEPM v p, _) -> (name, show v)
        (CEIf _, _) -> (name, ifEntity gr name n)
        (_, _) -> (name, "")
        where name = calcEntityName gr n

genTopEntity :: Gr CalcEntity EdgeRole -> (String, String)
genTopEntity gr = ("device", topEntity "device" gr)

genVHDL :: Gr CalcEntity EdgeRole -> IO ()
genVHDL gr = do
        let dir = "G:\\hsOut\\"
        createDirectoryIfMissing True dir
        --dcont <- getDirectoryContents dir
        --files <- filterM (\ fname -> doesFileExist $ dir ++ fname) dcont
        --forM_ files removeFile
        forM_ (genTopEntity gr : (map (getVHDLFile gr) $ labNodes gr)) (\ (name, content) -> do
        h <- openFile (dir ++ name ++ ".vhdl") WriteMode
        hPutStr h content
        hClose h)