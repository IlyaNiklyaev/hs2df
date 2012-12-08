module Core.CoreTypes where

import Type
import TyCon
import DataCon

data Channel = Tag String | Typed String Type

getChannelName :: Channel -> String
getChannelName (Tag name) = name
getChannelName (Typed name _) = name

getDataConIface :: String -> DataCon -> [Channel]
getDataConIface parentId dc = if 1 == length (dataConOrigArgTys dc) then (primitivizeType parentId $ head $ dataConOrigArgTys dc) else concatMap (\ (i,t) -> primitivizeType (parentId ++ show i) t) $ zip [0,1..] $ dataConOrigArgTys dc

primitivizeType :: String -> Type -> [Channel]
primitivizeType parentId t = if not $ isAlgType t then [Typed parentId t] else case tyConAppTyCon_maybe t of
                        Just tc -> if 1 == length (tyConDataCons tc) then (getDataConIface parentId $ head $ tyConDataCons tc) else (Tag $ parentId ++ "tag"):(concatMap (\ (i,dc) -> getDataConIface (parentId ++ show i) dc) $ zip [0,1..] $ tyConDataCons tc) 
                        Nothing -> [Typed parentId t]

primitivizeType' :: String -> Type -> [[Channel]]
primitivizeType' parentId t = if not $ isAlgType t then [[Typed parentId t]] else case tyConAppTyCon_maybe t of
                        Just tc -> if 1 == length (tyConDataCons tc) then [getDataConIface parentId $ head $ tyConDataCons tc] else [Tag $ parentId ++ "tag"]:(map (\ (i,dc) -> getDataConIface (parentId ++ show i) dc) $ zip [0,1..] $ tyConDataCons tc) 
                        Nothing -> [[Typed parentId t]]