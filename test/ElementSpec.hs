module ElementSpec (tests) where

import MahjongYaku.Element (ExtractedElement (..), extractRun, extractRunForAll, extractTriple)
import MahjongYaku.Tile (TileType (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Test.Tasty.Hspec ()

tests :: TestTree
tests =
  testGroup
    "牌についてのテスト"
    [ testExtractRun
    , testExtractAllRun
    , testExtractTriple
    ]

testExtractRun :: TestTree
testExtractRun =
  testGroup
    "順子の抽出"
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
        "順子が存在しない"
        [ testCase "3個の牌(1)" $ assertBool "可能性は存在しない" $ null (extractRun [C1, C2, C4] C1)
        ]
    , testGroup
        "一萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3] C1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [C4, C5, C6], exElement = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [C1, C2, C3, C1, C2, C3] C1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [C1, C2, C3], exElement = [C1, C2, C3]}) `elem` ps
        ]
    , testGroup
        "二萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3] C2
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C2
              assertEqual "可能性は2つ" 2 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [C4, C5, C6], exElement = [C1, C2, C3]}) `elem` ps
              assertBool "可能性(2)" $ (MkExElem {exRest = [C1, C5, C6], exElement = [C2, C3, C4]}) `elem` ps
        ]
    , testGroup
        "三萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3] C3
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [C1, C2, C3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C1, C2, C3, C4, C5, C6] C3
              assertEqual "可能性は3つ" 3 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [C4, C5, C6], exElement = [C1, C2, C3]}) `elem` ps
              assertBool "可能性(2)" $ (MkExElem {exRest = [C1, C5, C6], exElement = [C2, C3, C4]}) `elem` ps
              assertBool "可能性(3)" $ (MkExElem {exRest = [C1, C2, C6], exElement = [C3, C4, C5]}) `elem` ps
        ]
    , testGroup
        "七萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C7, C8, C9] C7
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C5, C6, C7, C7, C8, C9] C7
              assertEqual "可能性は2つ" 3 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exElement = [C7, C8, C9], exRest = [C5, C6, C7]}) `elem` ps
              assertBool "可能性(2)" $ (MkExElem {exElement = [C5, C6, C7], exRest = [C7, C8, C9]}) `elem` ps
              assertBool "可能性(3)" $ (MkExElem {exElement = [C6, C7, C8], exRest = [C5, C7, C9]}) `elem` ps
        ]
    , testGroup
        "八萬"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [C7, C8, C9] C8
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C4, C5, C6, C7, C8, C9] C8
              assertEqual "可能性は2つ" 2 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [C4, C5, C6], exElement = [C7, C8, C9]}) `elem` ps
              assertBool "可能性(2)" $ (MkExElem {exRest = [C4, C5, C9], exElement = [C6, C7, C8]}) `elem` ps
        ]
    , testGroup
        "九萬"
        [ testCase
            "3個の牌(1)"
            $ do
              let ps = extractRun [C7, C8, C9] C9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [C4, C5, C6, C7, C8, C9] C9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [C4, C5, C6], exElement = [C7, C8, C9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [C7, C8, C9, C7, C8, C9] C9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [C7, C8, C9], exElement = [C7, C8, C9]}) `elem` ps
        ]
    , testGroup
        "一筒"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [D1, D2, D3] D1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [D1, D2, D3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [D1, D2, D3, D4, D5, D6] D1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [D4, D5, D6], exElement = [D1, D2, D3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [D1, D2, D3, D1, D2, D3] D1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [D1, D2, D3], exElement = [D1, D2, D3]}) `elem` ps
        ]
    , testGroup
        "九筒"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [D7, D8, D9] D9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [D7, D8, D9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [D4, D5, D6, D7, D8, D9] D9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [D4, D5, D6], exElement = [D7, D8, D9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [D7, D8, D9, D7, D8, D9] D9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [D7, D8, D9], exElement = [D7, D8, D9]}) `elem` ps
        ]
    , testGroup
        "一索"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [B1, B2, B3] B1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [B1, B2, B3]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [B1, B2, B3, B4, B5, B6] B1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [B4, B5, B6], exElement = [B1, B2, B3]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [B1, B2, B3, B1, B2, B3] B1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [B1, B2, B3], exElement = [B1, B2, B3]}) `elem` ps
        ]
    , testGroup
        "九索"
        [ testCase "3個の牌(1)" $
            do
              let ps = extractRun [B7, B8, B9] B9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [], exElement = [B7, B8, B9]}) `elem` ps
        , testCase "6個の牌(1)" $
            do
              let ps = extractRun [B4, B5, B6, B7, B8, B9] B9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [B4, B5, B6], exElement = [B7, B8, B9]}) `elem` ps
        , testCase "6個の牌(2)" $
            do
              let ps = extractRun [B7, B8, B9, B7, B8, B9] B9
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exRest = [B7, B8, B9], exElement = [B7, B8, B9]}) `elem` ps
        ]
    ]

testExtractAllRun :: TestTree
testExtractAllRun =
  testGroup
    "順子をすべての数牌で検証する"
    [ testCase "(1)" $
        do
          let ps = extractRunForAll [B5, B6, B7, B7, B8, B9]
          assertEqual "可能性は3つ" 3 (length ps)
          assertBool "可能性(1)" $ (MkExElem {exElement = [B5, B6, B7], exRest = [B7, B8, B9]}) `elem` ps
          assertBool "可能性(2)" $ (MkExElem {exElement = [B6, B7, B8], exRest = [B5, B7, B9]}) `elem` ps
          assertBool "可能性(3)" $ (MkExElem {exElement = [B7, B8, B9], exRest = [B5, B6, B7]}) `elem` ps
    ]

testExtractTriple :: TestTree
testExtractTriple =
  testGroup
    "刻子の抽出"
    [ testGroup
        "最も単純なケース"
        [ testCase "1個の牌は必ず成立しない" $
            assertEqual "可能性" [] (extractTriple [C1] C1)
        , testCase "2個の牌では必ず成立しない" $
            assertEqual "可能性" [] (extractTriple [C1, C1] C1)
        , testCase "3個の牌" $
            do
              let ps = extractTriple [C1, C1, C1] C1
              assertEqual "可能性は1つ" 1 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exElement = [C1, C1, C1], exRest = []}) `elem` ps
        , testCase "4個の牌" $
            do
              let ps = extractTriple [C1, C1, C1, C1] C1
              assertEqual "可能性は2つ" 2 (length ps)
              assertBool "可能性(1)" $ (MkExElem {exElement = [C1, C1, C1], exRest = [C1]}) `elem` ps
              assertBool "可能性(2)" $ (MkExElem {exElement = [C1, C1, C1, C1], exRest = []}) `elem` ps
        ]
    , testCase "3個の牌で成立しないケース(1)" $
        assertEqual "可能性" [] (extractTriple [C1, C2, C3] C1)
    ]
