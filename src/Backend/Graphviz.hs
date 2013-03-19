module Backend.Graphviz where

import Data.Graph.Inductive
import CoreGraph

printGraph :: Gr String String -> String
printGraph gr =  graphviz gr "infograph" (8.5, 11) (1, 1) Portrait

showGraphNodes :: Gr CalcEntity () -> Gr String String
showGraphNodes = (nmap show).(emap (\ _ -> ""))

insRoot :: Gr String String -> Gr String String
insRoot gr = let [nnode] = newNodes 1 gr in insEdge (head $ rdfs' gr,nnode,"data") $ insNode (nnode,"out") gr

addChannelInterfaces :: Gr String String -> Gr String String
addChannelInterfaces gr = insEdges (concatMap (\ (from,to) -> [(from,to,"ack"),(to,from,"first"),(to,from,"next")]) (edges gr)) gr

genGraphviz :: Gr CalcEntity () -> IO ()
genGraphviz = putStrLn.printGraph.showGraphNodes