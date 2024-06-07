// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=swift-5.9)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=upcoming-swift)
//
// REQUIRES: executable_test

import StdlibUnittest
import UsingBaseMembers

var UsingBaseTestSuite = TestSuite("Using Base Members")

UsingBaseTestSuite.test("PublicBasePrivateInheritance") {
  var p = PublicBasePrivateInheritance()
  expectEqual(123, p.publicGetter())
  p.publicSetter(456)
  expectEqual(456, p.publicGetter())
}

UsingBaseTestSuite.test("PublicBaseProtectedInheritance") {
  var p = PublicBaseProtectedInheritance()
  expectEqual(123, p.publicGetter())
  p.publicSetter(987)
  expectEqual(987, p.publicGetter())
}

UsingBaseTestSuite.test("UsingBaseConstructorWithParam") {
  let p1 = UsingBaseConstructorWithParam(566 as Int32)
  expectEqual(566, p1.value)
  let p2 = UsingBaseConstructorWithParam(987 as UInt32)
  expectEqual(987, p2.value)
}

UsingBaseTestSuite.test("UsingBaseConstructorEmpty") {
  let p = UsingBaseConstructorEmpty()
  expectEqual(456, p.value)
}

runAllTests()
