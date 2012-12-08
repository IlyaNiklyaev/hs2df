module Backend.VHDL.Literal where

import Literal
import FastString (n_bytes, unpackFS)
import Numeric (showIntAtBase, fromRat)
import Data.Char (intToDigit, ord)
import Text.Printf
import Backend.VHDL.BuiltIn.Types
import Backend.VHDL.Types
import Tools
import Data.List (intercalate)
import Data.Graph.Inductive
import Core.CoreGraph
import Backend.VHDL.Tools

literalBusWidth :: Literal -> Int
literalBusWidth (MachChar _) = 31
literalBusWidth (MachInt _) = 32
literalBusWidth (MachWord _) = 32
literalBusWidth (MachInt64 _) = 64
literalBusWidth (MachStr val) = ((n_bytes val) * 8)
literalBusWidth (LitInteger _ _) = 32
literalBusWidth (MachLabel _ _ _) = 32

literalBinaryValue :: Literal -> String
literalBinaryValue (MachChar val) = show val
literalBinaryValue (MachInt val) = show val
literalBinaryValue (MachWord val) = show val
literalBinaryValue (MachInt64 val) = show val
literalBinaryValue (MachFloat val) = show $ fromRat val
literalBinaryValue (MachDouble val) = show $ fromRat val
literalBinaryValue (MachStr val) = printf "%b" $ unpackFS val
literalBinaryValue (LitInteger val _) = showIntAtBase 2 intToDigit val ""
literalBinaryValue (MachLabel val _ _) = printf "%b" $ unpackFS val

alignBinaryValue :: String -> Int -> String
alignBinaryValue s n = (take (n - length s) $ repeat '0') ++ s

literalEntity :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> String
literalEntity gr ln@(_, CELit lit) name = unlines [
        "library IEEE;",
        "use IEEE.std_logic_1164.all;",
        "use IEEE.numeric_std.all;",
        "library IEEE_proposed;",
        "use IEEE_proposed.float_pkg.all;",
        "entity " ++ name ++ " is",
        "    Port (",
        intercalate ";\n" $ map (\ (n, d, t) -> "   " ++ n ++ " : " ++ d ++ " " ++ t) $ calcEntityPort gr ln,
        "          );",
        "end " ++ name ++ ";",
        "",
        "architecture Behavioral of " ++ name ++ " is",
        "",
        "begin",
        "",
        "ack <= first or nex;",
        "",
        "data <= to_" ++ busType ++ "(" ++ literalBinaryValue lit ++ if busType == "float" then ", data'high, -data'low);" else ", data'length);",
        "",
        "end Behavioral;"
        ] where
                busWidth = (sHigh.snd.getTypeIface.literalType) lit
                busType = (sType.snd.getTypeIface.literalType) lit