// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none -Xfrontend -disable-llvm-verify -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import StdlibUnittest
import Singleton

var SingletonTestSuite = TestSuite("Singleton types that are marked as foreign references")

SingletonTestSuite.test("DeletedDtor") {
  var x = DeletedDtor.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  mutateIt(x)
  expectEqual(x.test(), 32)

  x = DeletedDtor.create()
  expectEqual(x.test(), 42)

  x.value = 22
  expectEqual(x.value, 22)
  expectEqual(x.test(), 22)
}

SingletonTestSuite.test("PrivateDtor") {
  var x = PrivateDtor.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  mutateIt(x)
  expectEqual(x.test(), 32)

  x = PrivateDtor.create()
  expectEqual(x.test(), 42)

  x.value = 22
  expectEqual(x.value, 22)
  expectEqual(x.test(), 22)
}

SingletonTestSuite.test("DeletedSpecialMembers") {
  var x = DeletedSpecialMembers.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  mutateIt(x)
  expectEqual(x.test(), 32)

  x = DeletedSpecialMembers.create()
  expectEqual(x.test(), 42)

  x.value = 22
  expectEqual(x.value, 22)
  expectEqual(x.test(), 22)
}

SingletonTestSuite.test("PrivateSpecialMembers") {
  var x = PrivateSpecialMembers.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  mutateIt(x)
  expectEqual(x.test(), 32)

  x = PrivateSpecialMembers.create()
  expectEqual(x.test(), 42)

  x.value = 22
  expectEqual(x.value, 22)
  expectEqual(x.test(), 22)
}

runAllTests()
