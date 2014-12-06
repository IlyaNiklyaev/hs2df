module Backend.VHDL.TopEntity where

import Data.Graph.Inductive
import Core.CoreGraph
import Graph.Tools
import Data.Graph.Analysis.Algorithms.Directed
import Data.List
import Backend.Common.Tools
import Backend.VHDL.Types
import Backend.VHDL.Tools
import Tools

edgePortName :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> LNode CalcEntity -> String -> String -> String
edgePortName gr from to sFrom sTo = calcEntityName gr from ++ "_" ++ sFrom ++ "_" ++ calcEntityName gr to ++ "_" ++ sTo

topEntity :: String -> Gr CalcEntity EdgeRole -> String
topEntity name gr = unlines [
        "library IEEE;",
        "use IEEE.std_logic_1164.all;",
        "use IEEE.numeric_std.all;",
        "library IEEE_proposed;",
        "use IEEE_proposed.float_pkg.all;",
        "entity " ++ name ++ " is",
        concat ["    Port (\n",
        "           first : in  std_logic;\n",
        "           nex : in  std_logic;\n",
        "           data : out  " ++ busType ++ " (" ++  show busHigh ++ " downto " ++  show busLow ++ ");\n",
        "           ack : out  std_logic",
        concatMap (\ (t, i) -> concat [
                ";\n           f" ++ show i ++ " : out  std_logic;\n",
                "           n" ++ show i ++ " : out  std_logic;\n",
                "           d" ++ show i ++ " : in  " ++ sType t ++ " (" ++  show (sHigh t) ++ " downto " ++  show (sLow t) ++ ");\n",
                "           a" ++ show i ++ " : in  std_logic"
                ]) $ zip (map (calcEntityTypeIface gr) params) [0,1..]],
        "          );",
        "end " ++ name ++ ";",
        "",
        "architecture Behavioral of " ++ name ++ " is",
        concatMap (\ node -> concat [
                "    COMPONENT " ++ (calcEntityName gr node) ++ "\n",
                "    PORT(",
                intercalate ";\n" $ map (\ (n, d, t) -> "        " ++ n ++ " : " ++ d ++ " " ++ t) $ calcEntityPort gr node,
                "\n        );\n",
                "    END COMPONENT;\n"]) $ labNodes gr,
        concat $ concatMap (\ (from, to, pm) -> map (\(f, t, tp) -> "signal " ++ edgePortName gr from to f t ++ ": " ++ tp ++ ";\n") pm) nodeMap,
        "begin",
        "",
        concat [
                "    top: " ++ (calcEntityName gr top) ++ " PORT MAP (\n",
                "        first => first,\n",
                "        nex => nex,\n",
                "        ack => ack,\n",
                "        data => data",
                concat $ concatMap (\ (from, to, pm) -> map (\(f, t, _) -> ",\n        " ++ t ++ " => " ++ edgePortName gr from to f t) pm) $ filter (\ (_,t,_) -> t == top) nodeMap,
                "\n        );"],
        concatMap (\ (node@(i, _), j) -> concat [
                "    inst" ++ show i ++ ": " ++ (calcEntityName gr node) ++ " PORT MAP (\n",
                "        f0 => f" ++ show j ++ ",\n",
                "        n0 => n" ++ show j ++ ",\n",
                "        d0 => d" ++ show j ++ ",\n",
                "        a0 => a" ++ show j ++ "",
                concat $ concatMap (\ (from, to, pm) -> map (\(f, t, _) -> ",\n        " ++ f ++ " => " ++ edgePortName gr from to f t) pm) $ filter (\ (f,_,_) -> f == node) nodeMap,
                "\n        );\n"]
                ) $ zip params [0,1..],
        concatMap (\ node@(i, _) -> concat [
                "    inst" ++ show i ++ ": " ++ (calcEntityName gr node) ++ " PORT MAP (",
                safeInit $ concat $ concat [
                        concatMap (\ (from, to, pm) -> map (\(f, t, _) -> "\n        " ++ f ++ " => " ++ edgePortName gr from to f t ++ ",") pm) $ filter (\ (f,_,_) -> f == node) nodeMap,
                        concatMap (\ (from, to, pm) -> map (\(f, t, _) -> "\n        " ++ t ++ " => " ++ edgePortName gr from to f t ++ ",") pm) $ filter (\ (_,t,_) -> t == node) nodeMap],
                "\n        );\n"]
                ) internals,
        "",
        "end Behavioral;"
        ] where
                busType = (sType.calcEntityTypeIface gr.head.leavesOf) gr
                busHigh = (sHigh.calcEntityTypeIface gr.head.leavesOf) gr
                busLow = (sLow.calcEntityTypeIface gr.head.leavesOf) gr
                params = filter (isParamLN gr) $ labNodes gr
                top = head $ leavesOf gr
                nodeMap = map (getEdgePortMap gr) $ labEdges gr
                internals = (labNodes gr) \\ (top:params)