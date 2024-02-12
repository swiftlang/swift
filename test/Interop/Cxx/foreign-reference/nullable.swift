// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none -Xfrontend -disable-llvm-verify -g -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import StdlibUnittest
import Nullable

var NullableTestSuite = TestSuite("Foreign references that are nullable")

NullableTestSuite.test("Empty") {
  var x = Empty.create()
  expectEqual(x!.test(), 42)

  mutateIt(x!)

  x = Empty.create() ?? Empty.create()!
  expectEqual(x!.test(), 42)
}

NullableTestSuite.test("IntPair") {
  var x = IntPair.create()
  expectEqual(x!.test(), 1)

  mutateIt(x)
  expectEqual(x!.test(), 2)

  x!.b = 42
  expectEqual(x!.test(), 40)

  x = IntPair.create() ?? IntPair.create()!
  expectEqual(x!.test(), 1)
}

runAllTests()
