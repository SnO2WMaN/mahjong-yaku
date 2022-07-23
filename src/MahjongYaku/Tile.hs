module MahjongYaku.Tile where

data DotTile
  = P1
  | P2
  | P3
  | P4
  | P5
  | P6
  | P7
  | P8
  | P9
data BambooTile
  = B1
  | B2
  | B3
  | B4
  | B5
  | B6
  | B7
  | B8
  | B9
data CharacterTile
  = C1
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
data WindTile -- 風牌
  = E -- 東
  | S -- 南
  | W -- 西
  | N -- 北
data DragonTile -- 字牌
  = Red -- 中
  | Green -- 發
  | White -- 白

data Tile
  = DotTile
  | BambooTile
  | CharacterTile
  | WindTile
  | DragonTile
