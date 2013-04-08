module Backend.VHDL.Function where

import Var
import Backend.VHDL.BuiltIn.Types
import Backend.VHDL.BuiltIn.Functions
import Backend.VHDL.Types

funcPort :: Var -> Port
funcPort var = [
        ("first", "in",  "std_logic"),
        ("nex", "in",  "std_logic"),
        ("data", "out",  oBusType ++ " (" ++  show (oBusWidth - 1) ++ " downto 0)"),
        ("ack", "out",  "std_logic")
        ] ++ (concatMap (\ (t, a, i) -> [
                ("f" ++ show i, "out","std_logic"),
                ("n" ++ show i, "out","std_logic"),
                ("d" ++ show i, "in",t ++ " (" ++  show (a - 1) ++ " downto 0)"),
                ("a" ++ show i, "in","std_logic")
                ]) $ zip3 iBusT iBusW [0,1..]) where
                oBusType = (sType.snd.getTypeIface.varType) var
                oBusWidth = (sArity.snd.getTypeIface.varType) var
                iBusT = map sType $ (fst.getTypeIface.varType) var
                iBusW = map sArity $ (fst.getTypeIface.varType) var

funcEntity :: String -> Var -> String
funcEntity name var = unlines [
        "library IEEE;",
        "use IEEE.std_logic_1164.all;",
        "use IEEE.numeric_std.all;",
        "entity " ++ name ++ " is",
        "    Port (",
        init $ concatMap (\ (n, d, t) -> "\n        " ++ n ++ " : " ++ d ++ " " ++ t ++ ";") $ funcPort var,
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
        (foldl (\ s i -> s ++ " and a" ++ show i) "ack <= a0" $ take (argCount - 1) [1,2..]) ++ ";",
        "",
        getFuncBody var,
        "",
        "end Behavioral;"
        ] where
                argCount = length $ (fst.getTypeIface.varType) var