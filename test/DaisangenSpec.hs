module DaisangenSpec where

import MahjongYaku.Daisangen (hasDaisangen)
import Test.Tasty.HUnit ((@?=))

unit_case1 = hasDaisangen True @?= True
