module Backend.VHDL.Param where

import Var
import Backend.VHDL.BuiltIn.Types
import Backend.VHDL.Types
import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph
import Backend.VHDL.Tools

paramEntity ::  Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
paramEntity gr ln@(n,_) name = unlines [
        "library IEEE;",
        "use IEEE.std_logic_1164.all;",
        "use IEEE.numeric_std.all;",
        "library IEEE_proposed;",
        "use IEEE_proposed.float_pkg.all;",
        "entity " ++ name ++ " is",
        "    Port (",
        intercalate ";\n" $ map (\ (n, d, t) -> "        " ++ n ++ " : " ++ d ++ " " ++ t) $ calcEntityPort gr ln,
        "          );",
        "end " ++ name ++ ";",
        "",
        "architecture Behavioral of " ++ name ++ " is",
        "",
        "begin",
        "",
        (foldl (\ s i -> s ++ " or first" ++ show i) "f0 <= first0" [1..oCount - 1]) ++ ";",
        (foldl (\ s i -> s ++ " or nex" ++ show i) "n0 <= nex0" [1..oCount - 1]) ++ ";",
        concatMap (\ i -> unlines [
        "ack" ++ show i ++ " <= a0;",
        "data" ++ show i ++ " <= d0;"]) [0..oCount - 1],
        "",
        "end Behavioral;"
        ] where
                oCount = length $ out gr n