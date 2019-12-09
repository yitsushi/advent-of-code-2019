module IntcodeSpec where

import Intcode
import qualified IntcodeTests.AddCommandSpec
import qualified IntcodeTests.AdjustBaseCommandSpec
import qualified IntcodeTests.EqualsCommandSpec
import qualified IntcodeTests.ExecuteSpec
import qualified IntcodeTests.InputCommandSpec
import qualified IntcodeTests.JumpIfFalseCommandSpec
import qualified IntcodeTests.JumpIfTrueCommandSpec
import qualified IntcodeTests.LessThenCommandSpec
import qualified IntcodeTests.MulCommandSpec
import qualified IntcodeTests.OutputCommandSpec
import qualified IntcodeTests.ParseSpec
import qualified IntcodeTests.ReadOpCodeSpec
import qualified IntcodeTests.ReplaceRegisterSpec
import qualified IntcodeTests.ResolveValueSpec
import Test.Hspec

spec :: Spec
spec = do
  IntcodeTests.AddCommandSpec.spec
  IntcodeTests.AdjustBaseCommandSpec.spec
  IntcodeTests.EqualsCommandSpec.spec
  IntcodeTests.InputCommandSpec.spec
  IntcodeTests.JumpIfFalseCommandSpec.spec
  IntcodeTests.JumpIfTrueCommandSpec.spec
  IntcodeTests.LessThenCommandSpec.spec
  IntcodeTests.MulCommandSpec.spec
  IntcodeTests.OutputCommandSpec.spec
  IntcodeTests.ParseSpec.spec
  IntcodeTests.ReadOpCodeSpec.spec
  IntcodeTests.ReplaceRegisterSpec.spec
  IntcodeTests.ResolveValueSpec.spec
  IntcodeTests.ExecuteSpec.spec
