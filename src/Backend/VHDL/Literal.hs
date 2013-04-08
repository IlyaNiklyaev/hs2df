module Backend.VHDL.Literal where

import Literal
import FastString (n_bytes, unpackFS)
import Numeric (showIntAtBase)
import Data.Char (intToDigit, ord)
import Text.Printf
import Backend.VHDL.BuiltIn.Types
import Backend.VHDL.Types

literalBusWidth :: Literal -> Int
literalBusWidth (MachChar _) = 31
literalBusWidth (MachInt _) = 32
literalBusWidth (MachWord _) = 32
literalBusWidth (MachInt64 _) = 64
literalBusWidth (MachStr val) = ((n_bytes val) * 8)
literalBusWidth (LitInteger _ _) = 32
literalBusWidth (MachLabel _ _ _) = 32

literalBinaryValue :: Literal -> String
literalBinaryValue (MachChar val) = showIntAtBase 2 intToDigit (ord val) ""
literalBinaryValue (MachInt val) = showIntAtBase 2 intToDigit val ""
literalBinaryValue (MachWord val) = showIntAtBase 2 intToDigit val ""
literalBinaryValue (MachInt64 val) = showIntAtBase 2 intToDigit val ""
literalBinaryValue (MachStr val) = printf "%b" $ unpackFS val
literalBinaryValue (LitInteger val _) = showIntAtBase 2 intToDigit val ""
literalBinaryValue (MachLabel val _ _) = printf "%b" $ unpackFS val

alignBinaryValue :: String -> Int -> String
alignBinaryValue s n = (take (n - length s) $ repeat '0') ++ s

literalPort :: Literal -> Port
literalPort lit = [
        ("first", "in",  "std_logic"),
        ("nex", "in",  "std_logic"),
        ("data", "out",  busType ++ " (" ++  show (busWidth - 1) ++ " downto 0)"),
        ("ack", "out",  "std_logic")
        ] where
                busWidth = (sArity.snd.getTypeIface.literalType) lit
                busType = (sType.snd.getTypeIface.literalType) lit

literalEntity :: String -> Literal -> String
literalEntity name lit = unlines [
        "library IEEE;",
        "use IEEE.std_logic_1164.all;",
        "use IEEE.numeric_std.all;",
        "entity " ++ name ++ " is",
        "    Port (",
        init $ concatMap (\ (n, d, t) -> "\n   " ++ n ++ " : " ++ d ++ " " ++ t ++ ";") $ literalPort lit,
        "          );",
        "end " ++ name ++ ";",
        "",
        "architecture Behavioral of " ++ name ++ " is",
        "",
        "begin",
        "",
        "ack <= first or nex;",
        "",
        "data <= \"" ++ alignBinaryValue (literalBinaryValue lit) busWidth ++ "\";",
        "",
        "end Behavioral;"
        ] where
                busWidth = (sArity.snd.getTypeIface.literalType) lit