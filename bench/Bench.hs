import Criterion.Main

import qualified Data.List.Split
import qualified Lib

main =
  defaultMain
    [ bgroup
        "lpad"
        [ bench "50" $ whnf Lib.lpad (50, '0', "1")
        , bench "100" $ whnf Lib.lpad (100, '0', "1")
        ]
    , bgroup
        "splitOn"
        [ bench "Lib" $ nf (Lib.splitOn ' ') (unwords $ replicate 50 "a")
        , bench "Data.List.Split" $
          nf (Data.List.Split.splitOn " ") (unwords $ replicate 50 "a")
        ]
    ]
