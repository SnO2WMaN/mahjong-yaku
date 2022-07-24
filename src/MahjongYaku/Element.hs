{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ordNub" #-}

module MahjongYaku.Element where

import Data.List (delete, nub)
import GHC.Enum (Bounded)
import MahjongYaku.Tile (TileType (..))
import Relude (catMaybes, group, sort)
import Relude.Extra (safeToEnum)

data ExtractedElement = MkExElem {exElement :: [TileType], exRest :: [TileType]} deriving (Eq, Show)

deleteElement :: TileType -> TileType -> TileType -> [TileType] -> [TileType]
deleteElement t1 t2 t3 ts = delete t1 $ delete t2 $ delete t3 ts

includeElement :: TileType -> TileType -> TileType -> [TileType] -> Bool
includeElement t1 t2 t3 ts = t1 `elem` ts && t2 `elem` ts && t3 `elem` ts

extractRun :: [TileType] -> TileType -> [ExtractedElement]
extractRun ts East = []
extractRun ts South = []
extractRun ts West = []
extractRun ts North = []
extractRun ts White = []
extractRun ts Green = []
extractRun ts Red = []
extractRun [] t = []
extractRun ts t =
  if t `notElem` ts
    then []
    else
      let mm2 = if t == C1 || t == D1 || t == B1 || t == C2 || t == D2 || t == B2 then Nothing else safeToEnum @TileType (fromEnum t - 2)
          mm1 = if t == C1 || t == D1 || t == B1 then Nothing else safeToEnum @TileType (fromEnum t - 1)
          mp1 = if t == C9 || t == D9 || t == B9 then Nothing else safeToEnum @TileType (fromEnum t + 1)
          mp2 = if t == C9 || t == D9 || t == B9 || t == C8 || t == D8 || t == B8 then Nothing else safeToEnum @TileType (fromEnum t + 2)
          mk t1 t2 t3 ts' = if includeElement t1 t2 t3 ts' then Just MkExElem {exElement = [t1, t2, t3], exRest = deleteElement t1 t2 t3 ts'} else Nothing
          pos1 = do
            m2 <- mm2
            m1 <- mm1
            mk m2 m1 t ts
          pos2 = do
            m1 <- mm1
            p1 <- mp1
            mk m1 t p1 ts
          pos3 = do
            p1 <- mp1
            p2 <- mp2
            mk t p1 p2 ts
       in catMaybes [pos1, pos2, pos3]

extractRunForAll :: [TileType] -> [ExtractedElement]
extractRunForAll rs =
  nub (concatMap (extractRun rs) [C1, C2, C3, C4, C5, C6, C7, C8, C9, D1, D2, D3, D4, D5, D6, D7, D8, D9, B1, B2, B3, B4, B5, B6, B7, B8, B9])

data Posibility = MkPosibility {posElements :: [[TileType]], posRest :: [TileType]} deriving (Eq, Show)

startR :: Posibility -> [Posibility]
startR pos = map (\ex -> MkPosibility {posElements = posElements pos ++ [exElement ex], posRest = exRest ex}) (extractRunForAll (posRest pos))

start :: [TileType] -> [Posibility]
start ts =
  concatMap startR $ concatMap startR $ concatMap startR $ startR MkPosibility {posElements = [], posRest = ts}
