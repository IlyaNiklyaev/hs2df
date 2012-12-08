{-# LANGUAGE FlexibleInstances #-}
module Core.CoreTree where

import CoreSyn
import Data.Tree
import Var
import Type
import Outputable (showSDoc)
import Data.List (find, findIndex)
import Data.Monoid
import DataCon (dataConUserType, dataConName)
import Core.CoreTools
import Data.Maybe (fromJust)
import Tools

data CoreNode m = CNBind (Bind m) | CNExpr (Expr m) | CNName m | CNAlt (AltCon, [m], Expr m) | CNPM m Int deriving Eq

instance (Show m) => Show (CoreNode m) where
        show (CNBind x) = show x
        show (CNExpr x) = show x
        show (CNName m) = show m
        show (CNPM m i) = "PM [" ++ show i ++ "]: " ++ show m
        show _ = ""

type CoreASTMap = [(String, [String], Tree (CoreNode CoreBndr))]

class CoreTree a where
        toTree :: a -> Tree a
        
mkCNAltCon :: (Alt m) -> CoreNode m
mkCNAltCon (alt, binds, expr) = CNAlt (alt, binds, expr)

instance CoreTree (CoreNode CoreBndr) where
        toTree (CNExpr (App expr arg)) = Node (CNExpr (App expr arg)) [toTree (CNExpr expr),toTree (CNExpr arg)]
        toTree (CNExpr (Lam b expr)) = Node (CNExpr (Lam b expr)) [toTree (CNName b),toTree (CNExpr expr)]
        toTree (CNExpr (Let bind expr)) = Node (CNExpr (Let bind expr)) [toTree (CNBind bind),toTree (CNExpr expr)]
        toTree (CNExpr (Case expr b t alts)) = Node (CNExpr (Case expr b t alts)) (toTree (CNExpr expr):(map (toTree.mkCNAltCon) alts))
        toTree (CNBind (NonRec b expr)) = Node (CNBind (NonRec b expr)) [toTree (CNName b),toTree (CNExpr expr)]
        toTree (CNBind (Rec binds)) = Node (CNBind (Rec binds)) (map (\(name,expr) -> toTree (CNBind (NonRec name expr))) binds)
        toTree (CNName var) = Node (CNName var) []
        toTree (CNAlt (LitAlt lit, _, expr)) = Node (CNExpr (Lit lit)) [toTree (CNExpr expr)]
        toTree (CNAlt (DataAlt dcon, binds, expr)) =  Node (CNName (mkCoVar (dataConName dcon) (dataConUserType dcon))) (toTree (CNExpr expr):map (toTree.CNName) binds)
        toTree (CNAlt (DEFAULT, _, expr)) = toTree (CNExpr expr)
        toTree node = Node node []

isName :: Tree (CoreNode m) -> Bool
isName (Node (CNName _) []) = True
isName _ = False

bindToAST :: Tree (CoreNode CoreBndr) -> (String, [String], Tree (CoreNode CoreBndr))
bindToAST (Node (CNBind _) [Node (CNName var) [], Node (CNExpr (Lam _ _)) params]) = (getVarName var, map (\ (Node (CNName x) _) -> getVarName x) $ safeInit params, last params)
bindToAST (Node (CNBind _) [Node (CNName var) [], node]) = (getVarName var, [], node)
bindToAST node = ("unnamed", [], node)

lookupCoreAST :: CoreASTMap -> String -> Maybe (Tree (CoreNode CoreBndr))
lookupCoreAST program name = case find (\ (var,_,_) -> var == name) program of
        Just (_,_,res) -> Just res
        Nothing -> Nothing

lookupCoreASTPar :: CoreASTMap -> String -> Maybe ([String],(Tree (CoreNode CoreBndr)))
lookupCoreASTPar program name = case find (\ (var,_,_) -> var == name) program of
        Just (_,params,res) -> Just (params,res)
        Nothing -> Nothing

treeMap' :: (Tree a -> Tree a) -> Tree a -> Tree a
treeMap' f (Node val l) = f (Node val (map (treeMap' f) l))
treeMap2' f (Node val l) = let (Node nval nl) = f (Node val l) in (Node nval (map (treeMap2' f) nl))
--treeMap' f (Node val l) = let (Node nval nl) = f (Node val (map (treeMap' f) l)) in (Node nval (map (treeMap' f) nl))

treeFoldMap' :: (Monoid m) => (Tree a -> m) -> m -> Tree a -> m
treeFoldMap' f m n@(Node _ l) = m `mappend` foldl (treeFoldMap' f) (f n) l

foldApps' :: Tree (CoreNode m) -> Tree (CoreNode m)
foldApps' n = case n of
        (Node (CNExpr (App expr arg)) [Node (CNExpr (App _ _)) (x:xs),y]) -> (Node (CNExpr (App expr arg)) ((x:xs) ++ [y]))
        _ -> n

foldApps :: Tree (CoreNode m) -> Tree (CoreNode m)
foldApps = treeMap' foldApps'

nameApps' :: Tree (CoreNode m) -> Tree (CoreNode m)
nameApps' (Node (CNExpr (App _ _)) ((Node (CNExpr expr) []):params)) = (Node (CNExpr expr) params)
nameApps' n = n

nameApps :: Tree (CoreNode m) -> Tree (CoreNode m)
nameApps = treeMap' nameApps'

elimForAlls' :: Tree (CoreNode m) -> Tree (CoreNode m)
elimForAlls' n@(Node (CNExpr (Var var)) ((Node (CNExpr (Type t)) _):(Node (CNExpr (Var _)) _):args)) = if isForAllTy (varType var) then Node (CNExpr (Var (setVarType var (applyTy (varType var) t)))) args else n
elimForAlls' n = n

elimForAlls :: Tree (CoreNode m) -> Tree (CoreNode m)
elimForAlls = treeMap' elimForAlls'

substParams' :: [(String, Tree (CoreNode CoreBndr))] -> Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr)
substParams' paramList n@(Node (CNExpr (Var var)) params) = case lookup (getVarName var) paramList of
        Just (Node pval pparams) -> (Node pval (pparams ++ params))
        Nothing -> n
substParams' paramList n@(Node (CNExpr (Type t)) params) = case lookup (showSDoc $ pprType t) paramList of
        Just node -> node
        Nothing -> n
substParams' _ n = n

substParams :: [(String, Tree (CoreNode CoreBndr))] -> Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr)
substParams paramList = treeMap2' (substParams' paramList)

substApps' :: CoreASTMap -> Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr)
substApps' program (Node (CNExpr (Var var)) args) = case lookupCoreASTPar program (getVarName var) of
        Just (params,node) -> substParams (zip params args) $ substApps program node
        Nothing -> (Node (CNExpr (Var var)) args)
substApps' _ n = n

substApps :: CoreASTMap -> Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr)
substApps program = treeMap2' (substApps' program)

substLets' :: Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr)
substLets' (Node (CNExpr (Let _ _)) [bind, expr]) = substApps [bindToAST bind] expr
substLets' n = n

substLets :: Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr)
substLets = treeMap' substLets'

deleteAltBinds :: Tree (CoreNode m) -> Tree (CoreNode m)
deleteAltBinds (Node val@(CNName _) (expr:_)) = (Node val [expr])
deleteAltBinds n = n

deleteCaseBinds' :: Tree (CoreNode m) -> Tree (CoreNode m)
deleteCaseBinds' (Node (CNExpr (Case _ _ _ _)) [_,Node (CNName _) (expr:_)]) = expr
deleteCaseBinds' (Node cexpr@(CNExpr (Case _ _ _ _)) (cond:alts)) = (Node cexpr (cond: map deleteAltBinds alts))
deleteCaseBinds' n = n

deleteCaseBinds :: Tree (CoreNode m) -> Tree (CoreNode m)
deleteCaseBinds = treeMap' deleteCaseBinds'

mkCaseBinding :: Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr) -> [Tree (CoreNode CoreBndr)]
mkCaseBinding exprn@(Node (CNExpr expr) _) (Node (CNName dcon) (_:binds)) = map (\ n@(Node (CNName b) _) -> (Node (CNBind (NonRec b expr)) [Node (CNName b) [], Node (CNPM dcon $ fromJust $ findIndex (== n) binds) [exprn]])) binds
mkCaseBinding _ _ = []

extractBinds' :: Tree (CoreNode CoreBndr) -> CoreASTMap
--extractBinds' (Node (CNExpr (Let _ _)) [bind, _]) = [bindToAST bind]
extractBinds' (Node (CNExpr (Case _ b _ _)) (expr:alts)) = (getVarName b, [], expr):(map bindToAST $ concatMap (mkCaseBinding expr) alts)
extractBinds' _ = []

extractBinds :: Tree (CoreNode CoreBndr) -> CoreASTMap
extractBinds t = treeFoldMap' extractBinds' [] t

splitAlts :: [Tree (CoreNode m)] -> [Tree (CoreNode m)]
splitAlts = foldl (\ alts (Node val alt) -> alts ++ (Node val []) : alt) []

splitCaseAlts' :: Tree (CoreNode m) -> Tree (CoreNode m)
splitCaseAlts' (Node cexpr@(CNExpr (Case _ _ _ _)) (expr@(Node (CNExpr _) _):def@(Node (CNExpr (Var _)) _):alts)) = Node cexpr (expr:def: splitAlts alts)
splitCaseAlts' (Node cexpr@(CNExpr (Case _ _ _ _)) (expr@(Node (CNExpr _) _):alts)) = Node cexpr (expr: splitAlts alts)
splitCaseAlts' n = n

splitCaseAlts :: Tree (CoreNode m) -> Tree (CoreNode m)
splitCaseAlts = treeMap' splitCaseAlts'

simplifyApps' :: Tree (CoreNode m) -> Tree (CoreNode m)
simplifyApps' (Node (CNExpr (App expr arg)) ((Node (CNExpr (App _ _)) [Node (CNExpr (Var var)) [],Node (CNExpr (Type _)) []]):(Node (CNExpr (Var _)) []):xs)) = (Node (CNExpr (App expr arg)) (Node (CNExpr (Var var)) []:xs))
simplifyApps' n = n

simplifyApps :: Tree (CoreNode m) -> Tree (CoreNode m)
simplifyApps = treeMap' simplifyApps'

foldLambdas' :: Tree (CoreNode m) -> Tree (CoreNode m)
foldLambdas' (Node (CNExpr (Lam _ _)) [name,(Node (CNExpr (Lam b2 expr2)) xs)]) = (Node (CNExpr (Lam b2 expr2)) (name:xs))
foldLambdas' n = n

foldLambdas :: Tree (CoreNode m) -> Tree (CoreNode m)
foldLambdas = treeMap' foldLambdas'

simplifyLambdas' :: Tree (CoreNode m) -> Tree (CoreNode m)
simplifyLambdas' (Node (CNExpr (Lam b expr)) (_:_:xs)) = (Node (CNExpr (Lam b expr)) xs)
simplifyLambdas' n = n

simplifyLambdas :: Tree (CoreNode m) -> Tree (CoreNode m)
simplifyLambdas = treeMap' simplifyLambdas'