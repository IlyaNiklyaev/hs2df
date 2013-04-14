module Backend.VHDL.Function where

import Var
import Backend.VHDL.BuiltIn.Types
import Backend.VHDL.BuiltIn.Functions
import Backend.VHDL.Types
import Backend.VHDL.Tools
import Tools
import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph

funcEntity :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
funcEntity gr ln@(_, ce) name = unlines [
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
        concatMap (\ i -> unlines [
                "\nf" ++ show i ++ " <= first;",
                "n" ++ show i ++ " <= nex;"
                ]) $ take argCount [0,1..],
        if argCount > 0 then (foldl (\ s i -> s ++ " and a" ++ show i) "ack <= a0" $ take (argCount - 1) [1,2..]) ++ ";" else "",
        "",
        getFuncBody var,
        "",
        "end Behavioral;"
        ] where
                argCount = length $ (fst.getTypeIface.varType) var
                var = case ce of
                        CEVar v -> v
                        CEExpr v -> v