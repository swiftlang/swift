// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none -Xfrontend -disable-llvm-verify -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import StdlibUnittest
import MoveOnly

@inline(never)
func blackHole(_ t: (BadCopyCtor, BadCopyCtor)) { }

var MoveOnlyTestSuite = TestSuite("Move only types that are marked as foreign references")

MoveOnlyTestSuite.test("MoveOnly") {
  var x = MoveOnly.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  x = MoveOnly.create()
  expectEqual(x.test(), 42)
}

MoveOnlyTestSuite.test("NoCopyMove") {
  var x = NoCopyMove.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  x = NoCopyMove.create()
  expectEqual(x.test(), 42)
}

MoveOnlyTestSuite.test("PrivateCopyCtor") {
  var x = PrivateCopyCtor.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  x = PrivateCopyCtor.create()
  expectEqual(x.test(), 42)
}

MoveOnlyTestSuite.test("BadCopyCtor") {
  var x = BadCopyCtor.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  x = BadCopyCtor.create()
  expectEqual(x.test(), 42)

  let t = (x, x) // Copy this around just to make sure we don't call the copy ctor.
  blackHole(t)
}

runAllTests()
