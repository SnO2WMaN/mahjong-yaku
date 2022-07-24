{-# LANGUAGE TypeApplications #-}

import Test.Tasty (defaultMain)
import TileSpec

main :: IO ()
main = defaultMain TileSpec.tests
