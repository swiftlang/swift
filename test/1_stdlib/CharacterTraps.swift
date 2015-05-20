// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release

import StdlibUnittest

var CharacterTraps = TestSuite("CharacterTraps")

CharacterTraps.test("CharacterFromEmptyString")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "unwrapping nil might or might not cause a crash in -Ounchecked mode"))
  .code {
  var s = ""
  expectCrashLater()
  Character(s)
}

CharacterTraps.test("CharacterFromMoreThanOneGraphemeCluster")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "unwrapping nil might or might not cause a crash in -Ounchecked mode"))
  .code {
  var s = "ab"
  expectCrashLater()
  Character(s)
}

runAllTests()

