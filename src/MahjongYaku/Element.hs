{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ordNub" #-}

module MahjongYaku.Element where

import Data.List (delete, nub)
import GHC.Enum (Bounded)
import MahjongYaku.Tile (TileType (..))
import Relude (IsList (fromList), catMaybes, group, head, last, sort)
import Relude.Extra (safeToEnum)
import Prelude hiding (head, last)

data ExtractedElement = MkExElem {exElement :: [TileType], exRest :: [TileType]} deriving (Eq, Show)

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
  if t `notElem` ts || length ts < 3
    then []
    else
      let mm2 = if t == C1 || t == D1 || t == B1 || t == C2 || t == D2 || t == B2 then Nothing else safeToEnum @TileType (fromEnum t - 2)
          mm1 = if t == C1 || t == D1 || t == B1 then Nothing else safeToEnum @TileType (fromEnum t - 1)
          mp1 = if t == C9 || t == D9 || t == B9 then Nothing else safeToEnum @TileType (fromEnum t + 1)
          mp2 = if t == C9 || t == D9 || t == B9 || t == C8 || t == D8 || t == B8 then Nothing else safeToEnum @TileType (fromEnum t + 2)
          mkRun t1 t2 t3 ts' = if t1 `elem` ts' && t2 `elem` ts' && t3 `elem` ts' then Just MkExElem {exElement = [t1, t2, t3], exRest = delete t1 $ delete t2 $ delete t3 ts'} else Nothing
          pos1 = do
            m2 <- mm2
            m1 <- mm1
            mkRun m2 m1 t ts
          pos2 = do
            m1 <- mm1
            p1 <- mp1
            mkRun m1 t p1 ts
          pos3 = do
            p1 <- mp1
            p2 <- mp2
            mkRun t p1 p2 ts
       in catMaybes [pos1, pos2, pos3]

extractRunForAll :: [TileType] -> [ExtractedElement]
extractRunForAll rs =
  nub (concatMap (extractRun rs) [C1, C2, C3, C4, C5, C6, C7, C8, C9, D1, D2, D3, D4, D5, D6, D7, D8, D9, B1, B2, B3, B4, B5, B6, B7, B8, B9])

extractTriple :: [TileType] -> TileType -> [ExtractedElement]
extractTriple ts t = case length (filter (== t) ts) of
  4 ->
    [ MkExElem {exElement = [t, t, t, t], exRest = delete t $ delete t $ delete t $ delete t ts}
    , MkExElem {exElement = [t, t, t], exRest = delete t $ delete t $ delete t ts}
    ]
  3 -> [MkExElem {exElement = [t, t, t], exRest = delete t $ delete t $ delete t ts}]
  _ -> []

extractTripleForAll :: [TileType] -> [ExtractedElement]
extractTripleForAll ts =
  nub (concatMap (extractTriple ts) [C1, C2, C3, C4, C5, C6, C7, C8, C9, D1, D2, D3, D4, D5, D6, D7, D8, D9, B1, B2, B3, B4, B5, B6, B7, B8, B9, East, South, West, North, White, Green, Red])

data Posibility = MkPosibility {posElements :: [[TileType]], posRest :: [TileType]} deriving (Eq, Show)

disassemblyOnce :: Posibility -> [Posibility]
disassemblyOnce pos =
  map
    (\ex -> MkPosibility {posElements = posElements pos ++ [exElement ex], posRest = exRest ex})
    (extractRunForAll (posRest pos) ++ extractTripleForAll (posRest pos))

-- disassembly [C1,C1,C1,C2,C3,C4,C5,C5,C6,C7,C8,C9,C9,C9]
disassembly :: [TileType] -> [Posibility]
disassembly ts =
  concatMap disassemblyOnce $
    concatMap disassemblyOnce $
      concatMap disassemblyOnce $
        disassemblyOnce (MkPosibility {posElements = [], posRest = ts})

data Yaku = Yaku {yakuElements :: [[TileType]], yakuHead :: [TileType]} deriving (Eq, Show)

isCompleted :: [TileType] -> [Yaku]
isCompleted ts =
  map
    (\pos -> Yaku {yakuElements = posElements pos, yakuHead = posRest pos})
    ( filter
        ( \pos ->
            let elements = posElements pos
                rest = posRest pos
             in length elements == 4 && length rest == 2 && head (fromList rest) == last (fromList rest)
        )
        (disassembly ts)
    )
