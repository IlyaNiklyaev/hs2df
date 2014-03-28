module Core.CoreTools where

import CoreSyn
import Literal
import Var
import Type
import Outputable (showSDoc)
import Name (nameOccName)
import OccName (occNameString)
import DataCon
import Id (isDataConId_maybe)
import TyCon (tyConDataCons)
import Data.List (find)

instance Show (Bind m) where
        show (NonRec _ _) = "NonRec"
        show (Rec _) = "Rec"

instance (Eq m, Variable m) => Eq (Bind m) where
        (NonRec b expr) == (NonRec b2 expr2) = and [expr == expr2, b == b2]
        (Rec v1) == (Rec v2) = v1 == v2
        _ == _ = False

showLit :: Literal -> String
showLit (MachChar val) =  show val
showLit (MachInt val) = show val
showLit (MachWord val) = show val
showLit (MachInt64 val) = show val
showLit (MachFloat val) = show val
showLit (MachDouble val) = show val
showLit (MachStr val) = show val
showLit (LitInteger val _) = show val
showLit (MachLabel val _ _) = show val
showLit _ = ""

instance Show (Expr m) where
        show (Lit lit) = showLit lit ++ ":" ++ (showSDoc $ pprType $ literalType lit)
        show (App expr arg) = "App"
        show (Lam b expr) = "/\\"
        show (Let bind expr) = "Let"
        show (Case expr b t alts) = "If"
        show (Var id) = getVarName id ++ ": " ++ (showSDoc $ pprType $ varType id)
        show (Cast expr coer) = "Cast"
        show (Tick tick expr) = "Tick"
        show (Type t) = "Type " ++ (showSDoc $ pprType t)
        show (Coercion coer) = "Coercion"
        show _ = "Other"

getAltNames :: (Variable m) => Alt m -> (AltCon, [String])
getAltNames (con, bs, expr) = (con, map getVarName bs)

instance (Eq m, Variable m) => Eq (Expr m) where
        (Lit lit) == (Lit lit2) = lit == lit2
        (App expr arg) == (App expr2 arg2) = and [expr == expr2, arg == arg2]
        (Lam b expr) == (Lam b2 expr2) = and [expr == expr2, getVarName b == getVarName b2]
        (Let bind expr) == (Let bind2 expr2) = and [expr == expr2, bind == bind2]
        (Case expr b t alts) == (Case expr2 b2 t2 alts2) = and [expr == expr2, getVarName b == getVarName b2, map getAltNames alts == map getAltNames alts2]
        (Var id) == (Var id2) = getVarName id == getVarName id2
        _ == _ = False

class Variable m where
        getVarName :: m -> String

instance Variable Var where
        getVarName = occNameString.nameOccName.varName