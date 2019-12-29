import Criterion.Main

import qualified Data.List.Split
import qualified Intcode as OI
import qualified IntcodeMachine as NI
import qualified Lib

tape :: String
tape =
  "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

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
    , bgroup
        "Intcode Machine"
        [ bench "Old" $ whnf OI.execute (OI.loadComputer tape [10])
        , bench "New" $ whnf NI.boot (NI.loadComputer tape [10])
        ]
    ]
