module Main where

import GHC 
import GHC.Paths ( libdir ) 
import DynFlags
import HscTypes
import CoreSyn
import System.Environment
import Control.Monad
import Control.Monad.Writer

import VHDLGenerator(vhdl)
import VHDLGenerator.DesignContext

import CoreTree
import Data.Tree (drawForest, Tree)

modifyAST :: Tree (CoreNode m) -> Tree (CoreNode m)
--modifyAST = simplifyLambdas.foldLambdas.simplifyApps.foldApps
modifyAST = foldLambdas.simplifyApps.foldApps

main :: IO ()
main = do
   (targetFile:_) <- getArgs
   res <- defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags dflags {importPaths = ["src\\"]}
        target <- guessTarget targetFile Nothing
        setTargets [target]
        _ <- load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "Main"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        let c = coreModule d
        let binds = mg_binds c
        return binds
   putStrLn.drawForest $ map ((fmap show).modifyAST.toTree.CNBind) res