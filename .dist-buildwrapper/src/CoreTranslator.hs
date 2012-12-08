module CoreTranslator where

import CoreSyn
import Language.VHDL.AST

coreBindParse :: Bind a -> SubProgBody

coreBindParse x = SubProgBody (Function (Basic "test") [] (Basic "testType")) [] []