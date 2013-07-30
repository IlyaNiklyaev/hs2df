module Backend.OpenCL where

import Data.Graph.Inductive
import Core.CoreGraph
import System.IO
import System.Directory

genOpenCL :: Gr CalcEntity EdgeRole -> IO ()
genOpenCL gr = do
        let dir = "G:\\hsOut\\"
        createDirectoryIfMissing True dir
        h <- openFile (dir ++ "kernels.cl") WriteMode
        hPutStr h ""
        hClose h