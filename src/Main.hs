{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import GHC 
import GHC.Paths ( libdir ) 
import DynFlags
import System.Environment

import Core.CoreTree
import Data.Tree
import Data.Graph.Inductive
import Data.Maybe (fromJust)
import CoreSyn (CoreBndr)
import Core.CoreGraph
import Backend.Graphviz
import Backend.VHDL.VHDL
import Backend.OpenCL.OpenCL
import Control.Monad (when)
import System.Console.GetOpt
import Options

modifyAST :: Tree (CoreNode CoreBndr) -> Tree (CoreNode CoreBndr)
modifyAST = substLets.elimForAlls.nameApps.foldLambdas.foldApps

modifyGraph :: Gr (CoreNode CoreBndr) EdgeRole -> Gr CalcEntity EdgeRole
modifyGraph = mergeDupNodes.(nmap mkCalcEntity)

setDynFlags :: [DynFlag] -> DynFlags -> DynFlags
setDynFlags fls dflags = foldl dopt_set dflags fls

unsetDynFlags :: [DynFlag] -> DynFlags -> DynFlags
unsetDynFlags fls dflags = foldl dopt_unset dflags fls

backends :: [(String, Gr CalcEntity EdgeRole -> String -> IO ())]
backends = [("vhdl", genVHDL), ("graph", genGraphviz), ("opencl", genOpenCL)]

main :: IO ()
main = do
   args <- getArgs
   let (opts,_,_) = getOpt Permute options args
   if (isValidOptions opts) then do
        res <- defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                _ <- setSessionDynFlags $ setDynFlags [] $ unsetDynFlags [] $ dflags {optLevel = 1}
                c <- compileToCoreSimplified $ sourceFile opts
                return $ cm_binds c
        let phase1 = map (modifyAST.toTree.CNBind) res
        when ("tree" `elem` (activeDumps opts)) $ putStrLn $ drawForest $ map (fmap show) phase1
        let intBinds = concatMap extractBinds phase1
        let phase2 = intBinds ++ map (bindToAST.splitCaseAlts.deleteCaseBinds) phase1
        let phase3 = modifyGraph.treeToGraph.(substApps phase2).fromJust $ lookupCoreAST phase2 "main"
        when ("binds" `elem` (activeDumps opts)) $ print $ map (\ (x, y, _) -> (x,y)) phase2
        sequence_ $ map (\ (_,x) -> x phase3 (outDirectory opts)) $ filter (\ (name, _) -> name `elem` (activeBackends opts)) backends
    else do
        putStrLn $ usageInfo "usage:" options