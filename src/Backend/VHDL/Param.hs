module Backend.VHDL.Param where

import Var
import Backend.VHDL.BuiltIn.Types
import Backend.VHDL.Types

paramPort :: Var -> Port
paramPort var = [
        ("first", "in",  "std_logic"),
        ("nex", "in",  "std_logic"),
        ("data", "out",  oBusType ++ " (" ++  show (oBusWidth - 1) ++ " downto 0)"),
        ("ack", "out",  "std_logic"),
        ("f0", "out",  "std_logic"),
        ("n0", "out",  "std_logic"),
        ("d0", "in",  oBusType ++ " (" ++  show (oBusWidth - 1) ++ " downto 0)"),
        ("a0", "in",  "std_logic")
        ] where
                oBusType = (sType.snd.getTypeIface.varType) var
                oBusWidth = (sArity.snd.getTypeIface.varType) var

paramEntity :: String -> Var -> String
paramEntity name var = unlines [
        "library IEEE;",
        "use IEEE.std_logic_1164.all;",
        "use IEEE.numeric_std.all;",
        "entity " ++ name ++ " is",
        "    Port (",
        init $ concatMap (\ (n, d, t) -> "\n        " ++ n ++ " : " ++ d ++ " " ++ t ++ ";") $ paramPort var,
        "          );",
        "end " ++ name ++ ";",
        "",
        "architecture Behavioral of " ++ name ++ " is",
        "",
        "begin",
        "",
        "f0 <= first;",
        "n0 <= nex;",
        "ack <= a0;",
        "data <= d0;",
        "",
        "end Behavioral;"
        ]