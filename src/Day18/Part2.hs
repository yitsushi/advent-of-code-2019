module Day18.Part2
  ( solve
  ) where

import Day18.Lib

{- Maybe approach
    1. Create a movement sequence based on the path
    2. Generate all possible init for A
    3. Drop each of them with length > 20
    4. Generate all possible init for B (drop length A)
    5. Drop each of them with length > 20
    6. Generate all possible init for C (drop length A + B)
    7. Drop each of them with length > 20
    8. Replace all appearance of A with RoutineA
    9. Replace all appearance of B with RoutineB
    10. Replace all appearance of C with RoutineC
    11. Remeaning list should contain only A, C and C
    12. Final list length < 20
    13. ???
    14. Profit

    Soon, but I'm not in the mood
-}
solve :: String -> String
solve "No Input" = "No Input Defined!"
solve input = "Day#18::Part2"
