module MahjongYaku.Tile where

data TileType
  = -- 萬子
    C1
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | -- 筒子
    D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  | -- 索子
    B1
  | B2
  | B3
  | B4
  | B5
  | B6
  | B7
  | B8
  | B9
  | East -- 東
  | South -- 南
  | West -- 西
  | North -- 北
  | White -- 白
  | Green -- 發
  | Red -- 中
  deriving (Eq, Ord, Show, Enum, Bounded)
