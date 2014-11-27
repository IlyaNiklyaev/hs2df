module Backend.Graphviz where

import Data.Graph.Inductive
import Core.CoreGraph
import System.IO
import System.Directory
import Data.Text.Lazy (unpack, pack)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

graphParams :: GraphvizParams Int String String () String
graphParams = nonClusteredParams {
                fmtNode = fn,
                fmtEdge = fe
        }
        where
                fn (_,l) = [(Label . StrLabel . pack) l]
                fe (_,_,l) = [(Label . StrLabel . pack) l]

printGraph :: Gr String String -> String
printGraph gr =  unpack $ printDotGraph $ graphToDot graphParams gr

showGraphNodes :: Gr CalcEntity EdgeRole -> Gr String String
showGraphNodes = (nmap show).(emap show)

insRoot :: Gr String String -> Gr String String
insRoot gr = let [nnode] = newNodes 1 gr in insEdge (head $ rdfs' gr,nnode,"data") $ insNode (nnode,"out") gr

addChannelInterfaces :: Gr String String -> Gr String String
addChannelInterfaces gr = insEdges (concatMap (\ (from,to) -> [(from,to,"ack"),(to,from,"first"),(to,from,"next")]) (edges gr)) gr

genGraphviz :: Gr CalcEntity EdgeRole -> String -> IO ()
genGraphviz gr dir = do
        createDirectoryIfMissing True dir
        h <- openFile (dir ++ "graph.dot") WriteMode
        hPutStr h $ printGraph $ showGraphNodes gr
        hClose h