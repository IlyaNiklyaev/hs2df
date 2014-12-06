module Backend.VHDL.If where

import Data.Graph.Inductive
import Core.CoreGraph
import Graph.Tools
import Backend.Common.Tools
import Backend.VHDL.Tools
import Data.List (intercalate)

ifEntity :: Gr CalcEntity EdgeRole -> String -> LNode CalcEntity -> String
ifEntity gr name n = unlines [
        "library IEEE;",
        "use IEEE.std_logic_1164.all;",
        "use IEEE.numeric_std.all;",
        "library IEEE_proposed;",
        "use IEEE_proposed.float_pkg.all;",
        "entity " ++ name ++ " is",
        "    Port (",
        intercalate ";\n" $ map (\ (n, d, t) -> "        " ++ n ++ " : " ++ d ++ " " ++ t) $ calcEntityPort gr n,
        "          );",
        "end " ++ name ++ ";",
        "",
        "architecture Behavioral of " ++ name ++ " is",
        "",
        "begin",
        "",
        concatMap (\ i -> unlines [
                "\nf" ++ show i ++ " <= first;",
                "n" ++ show i ++ " <= nex;"
                ]) $ take argCount [0,1..],
        concat [
                foldl (\ s i -> s ++ " else\n        d" ++ show (i + aCount) ++ " when d0 = d" ++ show i) ("    data <= d" ++ show (aCount + 1) ++ " when d0 = d1") $ take (aCount - 1) [2,3..],
                if hasDefault then " else\n        d" ++ show (aCount * 2 + 1) else "",
                ";"
        ],
        concat [
                foldl (\ s i -> s ++ " else\n        a" ++ show (i + aCount) ++ " when d0 = d" ++ show i) ("    ack <= a" ++ show (aCount + 1) ++ " when d0 = d1") $ take (aCount - 1) [2,3..],
                if hasDefault then " else\n        a" ++ show (aCount * 2 + 1) else "",
                ";"
        ],
        "end Behavioral;"
        ] where
                argCount = ((length $ calcEntityPort gr n) `div` 4) - 1
                hasDefault = ((length $ calcEntityPort gr n) `mod` 8) == 4
                aCount = altCount gr n