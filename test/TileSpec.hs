module TileSpec (tests) where

import MahjongYaku.Tile (
  Ext (Ext, element, rest),
  Tile (Tile),
  TileType (B1, B2, B3, B4, B5, B6, B7, B8, B9, C1, C2, C3, C4, C5, C6, C7, C8, C9, D1, D2, D3, D4, D5, D6, D7, D8, D9, East, Green, North, Red, South, West, White),
  extractRun,
 )

import Relude (isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Test.Tasty.Hspec ()

tests :: TestTree
tests = testGroup "牌についてのテスト" [testExtractRun]

testExtractRun :: TestTree
testExtractRun =
  testGroup
    "刻子の抽出"
    [ testGroup
        "字牌の検定はしない"
        [ testCase "東" $
            assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C3] East)
        , testCase "南" $
            assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C3] South)
        , testCase "西" $
            assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C3] West)
        , testCase "北" $
            assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C3] North)
        , testCase "白" $
            assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C3] White)
        , testCase "發" $
            assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C3] Green)
        , testCase "中" $
            assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C3] Red)
        ]
    , testCase "検定する牌が存在しない" $
        assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C3] C4)
    , testGroup
        "刻子が存在しない"
        [ testCase "3個の牌(1)" $ assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C4] C1)
        ]
    , testGroup
        "一萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3] C1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [C4, C5, C6], element = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [C1, C2, C3, C1, C2, C3] C1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [C1, C2, C3], element = [C1, C2, C3]}) `elem` ps
        ]
    , testGroup
        "二萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3] C2
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C2
              assertEqual "可能性は2つ" (length ps) 2
              assertBool "可能性(1)" $ (Ext {rest = [C4, C5, C6], element = [C1, C2, C3]}) `elem` ps
              assertBool "可能性(2)" $ (Ext {rest = [C1, C5, C6], element = [C2, C3, C4]}) `elem` ps
        ]
    , testGroup
        "三萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3] C3
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C3
              assertEqual "可能性は3つ" (length ps) 3
              assertBool "可能性(1)" $ (Ext {rest = [C4, C5, C6], element = [C1, C2, C3]}) `elem` ps
              assertBool "可能性(2)" $ (Ext {rest = [C1, C5, C6], element = [C2, C3, C4]}) `elem` ps
              assertBool "可能性(3)" $ (Ext {rest = [C1, C2, C6], element = [C3, C4, C5]}) `elem` ps
        ]
    , testGroup
        "八萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C7, C8, C9] C8
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C4, C5, C6, C7, C8, C9] C8
              assertEqual "可能性は2つ" (length ps) 2
              assertBool "可能性(1)" $ (Ext {rest = [C4, C5, C6], element = [C7, C8, C9]}) `elem` ps
              assertBool "可能性(2)" $ (Ext {rest = [C4, C5, C9], element = [C6, C7, C8]}) `elem` ps
        ]
    , testGroup
        "九萬"
        [ testCase
            "3個の牌(1)"
            $ do
              let ps = extractRun [C7, C8, C9] C9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C4, C5, C6, C7, C8, C9] C9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [C4, C5, C6], element = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [C7, C8, C9, C7, C8, C9] C9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [C7, C8, C9], element = [C7, C8, C9]}) `elem` ps
        ]
    , testGroup
        "一筒"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [D1, D2, D3] D1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [D1, D2, D3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [D1, D2, D3, D4, D5, D6] D1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [D4, D5, D6], element = [D1, D2, D3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [D1, D2, D3, D1, D2, D3] D1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [D1, D2, D3], element = [D1, D2, D3]}) `elem` ps
        ]
    , testGroup
        "九筒"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [D7, D8, D9] D9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [D7, D8, D9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [D4, D5, D6, D7, D8, D9] D9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [D4, D5, D6], element = [D7, D8, D9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [D7, D8, D9, D7, D8, D9] D9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [D7, D8, D9], element = [D7, D8, D9]}) `elem` ps
        ]
    , testGroup
        "一索"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [B1, B2, B3] B1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [B1, B2, B3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [B1, B2, B3, B4, B5, B6] B1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [B4, B5, B6], element = [B1, B2, B3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [B1, B2, B3, B1, B2, B3] B1
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [B1, B2, B3], element = [B1, B2, B3]}) `elem` ps
        ]
    , testGroup
        "九索"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [B7, B8, B9] B9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [], element = [B7, B8, B9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [B4, B5, B6, B7, B8, B9] B9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [B4, B5, B6], element = [B7, B8, B9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [B7, B8, B9, B7, B8, B9] B9
              assertEqual "可能性は1つ" (length ps) 1
              assertBool "可能性(1)" $ (Ext {rest = [B7, B8, B9], element = [B7, B8, B9]}) `elem` ps
        ]
    ]
