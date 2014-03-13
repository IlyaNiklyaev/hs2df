module Backend.OpenCL.OpenCL where

import Data.Graph.Inductive
import Core.CoreGraph
import Control.Monad
import System.IO
import System.Directory
import Backend.OpenCL.Host
import Backend.OpenCL.Kernel

genOpenCL :: Gr CalcEntity EdgeRole -> String -> IO ()
genOpenCL gr dir = do
        createDirectoryIfMissing True dir
        forM_ [genHostBody gr, genHostHeader gr, genKernels gr] (\ (name, content) -> do
        h <- openFile (dir ++ name) WriteMode
        hPutStr h content
        hClose h)