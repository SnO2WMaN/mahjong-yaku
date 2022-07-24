module ElementSpec (tests) where

import MahjongYaku.Element (Element (..), extractRun, extractRunForAll)
import MahjongYaku.Tile (TileType (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Test.Tasty.Hspec ()

tests :: TestTree
tests =
  testGroup
    "牌についてのテスト"
    [ testElementractRun
    , testElementractAllRun
    ]

testElementractRun :: TestTree
testElementractRun =
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
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [C4, C5, C6], element = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [C1, C2, C3, C1, C2, C3] C1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [C1, C2, C3], element = [C1, C2, C3]}) `elem` ps
        ]
    , testGroup
        "二萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3] C2
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C2
              assertEqual "可能性は2つ" 2 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [C4, C5, C6], element = [C1, C2, C3]}) `elem` ps
              assertBool "可能性(2)" $ (Element {rest = [C1, C5, C6], element = [C2, C3, C4]}) `elem` ps
        ]
    , testGroup
        "三萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3] C3
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C3
              assertEqual "可能性は3つ" 3 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [C4, C5, C6], element = [C1, C2, C3]}) `elem` ps
              assertBool "可能性(2)" $ (Element {rest = [C1, C5, C6], element = [C2, C3, C4]}) `elem` ps
              assertBool "可能性(3)" $ (Element {rest = [C1, C2, C6], element = [C3, C4, C5]}) `elem` ps
        ]
    , testGroup
        "七萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C7, C8, C9] C7
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C5, C6, C7, C7, C8, C9] C7
              assertEqual "可能性は2つ" 3 (length ps)
              assertBool "可能性(1)" $ (Element {element = [C7, C8, C9], rest = [C5, C6, C7]}) `elem` ps
              assertBool "可能性(2)" $ (Element {element = [C5, C6, C7], rest = [C7, C8, C9]}) `elem` ps
              assertBool "可能性(3)" $ (Element {element = [C6, C7, C8], rest = [C5, C7, C9]}) `elem` ps
        ]
    , testGroup
        "八萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C7, C8, C9] C8
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C4, C5, C6, C7, C8, C9] C8
              assertEqual "可能性は2つ" 2 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [C4, C5, C6], element = [C7, C8, C9]}) `elem` ps
              assertBool "可能性(2)" $ (Element {rest = [C4, C5, C9], element = [C6, C7, C8]}) `elem` ps
        ]
    , testGroup
        "九萬"
        [ testCase
            "3個の牌(1)"
            $ do
              let ps = extractRun [C7, C8, C9] C9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C4, C5, C6, C7, C8, C9] C9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [C4, C5, C6], element = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [C7, C8, C9, C7, C8, C9] C9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [C7, C8, C9], element = [C7, C8, C9]}) `elem` ps
        ]
    , testGroup
        "一筒"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [D1, D2, D3] D1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [D1, D2, D3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [D1, D2, D3, D4, D5, D6] D1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [D4, D5, D6], element = [D1, D2, D3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [D1, D2, D3, D1, D2, D3] D1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [D1, D2, D3], element = [D1, D2, D3]}) `elem` ps
        ]
    , testGroup
        "九筒"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [D7, D8, D9] D9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [D7, D8, D9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [D4, D5, D6, D7, D8, D9] D9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [D4, D5, D6], element = [D7, D8, D9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [D7, D8, D9, D7, D8, D9] D9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [D7, D8, D9], element = [D7, D8, D9]}) `elem` ps
        ]
    , testGroup
        "一索"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [B1, B2, B3] B1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [B1, B2, B3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [B1, B2, B3, B4, B5, B6] B1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [B4, B5, B6], element = [B1, B2, B3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [B1, B2, B3, B1, B2, B3] B1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [B1, B2, B3], element = [B1, B2, B3]}) `elem` ps
        ]
    , testGroup
        "九索"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [B7, B8, B9] B9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [], element = [B7, B8, B9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [B4, B5, B6, B7, B8, B9] B9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [B4, B5, B6], element = [B7, B8, B9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [B7, B8, B9, B7, B8, B9] B9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (Element {rest = [B7, B8, B9], element = [B7, B8, B9]}) `elem` ps
        ]
    ]

testElementractAllRun =
  testGroup
    "刻子をすべての数牌で検証する"
    [ testCase "(1)" $
        do
          let ps = extractRunForAll [B5, B6, B7, B7, B8, B9]
          assertEqual "可能性は3つ" 3 (length ps)
          assertBool "可能性(1)" $ (Element {element = [B5, B6, B7], rest = [B7, B8, B9]}) `elem` ps
          assertBool "可能性(2)" $ (Element {element = [B6, B7, B8], rest = [B5, B7, B9]}) `elem` ps
          assertBool "可能性(3)" $ (Element {element = [B7, B8, B9], rest = [B5, B6, B7]}) `elem` ps
    ]
