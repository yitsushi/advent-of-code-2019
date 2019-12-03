import Criterion.Main

import Lib

main =
  defaultMain
    [ bgroup
        "lpad"
        [ bench "1" $ whnf lpad (1, '0', "1")
        , bench "5" $ whnf lpad (5, '0', "1")
        , bench "9" $ whnf lpad (10, '0', "1")
        , bench "50" $ whnf lpad (50, '0', "1")
        , bench "100" $ whnf lpad (100, '0', "1")
        ]
    , bgroup
        "splitOn"
        [ bench "1" $ nf splitOn (' ', "a")
        , bench "5" $ nf splitOn (' ', unwords $ replicate 5 "a")
        , bench "10" $ nf splitOn (' ', unwords $ replicate 10 "a")
        , bench "50" $ nf splitOn (' ', unwords $ replicate 50 "a")
        , bench "100" $ nf splitOn (' ', unwords $ replicate 100 "a")
        ]
    ]
