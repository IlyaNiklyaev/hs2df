module Backend.Graphviz where

import Data.Graph.Inductive
import Core.CoreGraph

printGraph :: Gr String String -> String
printGraph gr =  graphviz gr "infograph" (8.5, 11) (1, 1) Portrait

showGraphNodes :: Gr CalcEntity EdgeRole -> Gr String String
showGraphNodes = (nmap show).(emap show)

insRoot :: Gr String String -> Gr String String
insRoot gr = let [nnode] = newNodes 1 gr in insEdge (head $ rdfs' gr,nnode,"data") $ insNode (nnode,"out") gr

addChannelInterfaces :: Gr String String -> Gr String String
addChannelInterfaces gr = insEdges (concatMap (\ (from,to) -> [(from,to,"ack"),(to,from,"first"),(to,from,"next")]) (edges gr)) gr

genGraphviz :: Gr CalcEntity EdgeRole -> IO ()
genGraphviz = putStrLn.printGraph.showGraphNodes