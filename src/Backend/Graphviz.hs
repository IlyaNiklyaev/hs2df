module Backend.Graphviz where

import Data.Graph.Inductive
import Core.CoreGraph
import System.IO
import System.Directory
import Data.Text.Lazy (unpack, pack, empty)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

graphParams :: GraphvizParams Int CalcEntity EdgeRole () CalcEntity
graphParams = nonClusteredParams {
                globalAttributes = ga,
                fmtNode = fn,
                fmtEdge = fe
        }
        where
                ga =    [
                                NodeAttrs [Shape BoxShape]
                        ]
                fn (_, CEDMerge _) = [(Label . StrLabel) Data.Text.Lazy.empty, Shape Triangle]
                fn (_,l) = [(Label . StrLabel . pack . show) l]
                fe (_,_,l) = [(Label . StrLabel . pack . show) l]

printGraph :: Gr CalcEntity EdgeRole -> String
printGraph gr =  unpack $ printDotGraph $ graphToDot graphParams gr

insRoot :: Gr String String -> Gr String String
insRoot gr = let [nnode] = newNodes 1 gr in insEdge (head $ rdfs' gr,nnode,"data") $ insNode (nnode,"out") gr

addChannelInterfaces :: Gr String String -> Gr String String
addChannelInterfaces gr = insEdges (concatMap (\ (from,to) -> [(from,to,"ack"),(to,from,"first"),(to,from,"next")]) (edges gr)) gr

genGraphviz :: Gr CalcEntity EdgeRole -> String -> IO ()
genGraphviz gr dir = do
        createDirectoryIfMissing True dir
        h <- openFile (dir ++ "graph.dot") WriteMode
        hPutStr h $ printGraph gr
        hClose h