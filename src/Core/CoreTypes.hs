module Core.CoreTypes where

import Type
import TyCon
import DataCon

data Channel = Tag Int | Typed Int Type

getDataConIface :: Int -> DataCon -> [Channel]
getDataConIface parentId dc = concatMap (primitivizeType parentId) $ dataConOrigArgTys dc

primitivizeType :: Int -> Type -> [Channel]
primitivizeType parentId t = if not $ isAlgType t then [Typed parentId t] else case tyConAppTyCon_maybe t of
                        Just tc -> (if 1 == length (tyConDataCons tc) then [] else [Tag parentId]) ++ (concatMap (getDataConIface parentId) $ tyConDataCons tc)
                        Nothing -> [Typed parentId t]