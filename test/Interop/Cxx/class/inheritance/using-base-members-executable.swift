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

UsingBaseTestSuite.test("PublicBaseUsingPrivateTypedef") {
  var p = PublicBaseUsingPrivateTypedef()
  expectEqual(123, p.publicGetter())
  p.publicSetter(987)
  expectEqual(987, p.publicGetter())
}

UsingBaseTestSuite.test("PublicBaseUsingPrivateUsingType") {
  var p = PublicBaseUsingPrivateTypedef()
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

UsingBaseTestSuite.test("ProtectedMemberPrivateInheritance") {
  let p = ProtectedMemberPrivateInheritance()
  expectEqual(456, p.protectedGetter())
}

UsingBaseTestSuite.test("OperatorBasePrivateInheritance") {
  let p = OperatorBasePrivateInheritance()
  expectTrue(Bool(fromCxx: p))
  expectTrue(Bool(fromCxx: !p)) // this looks a bit nonsensical because
                                // operator!() doesn't actually do anything,
                                // it's just the identity function
  expectEqual(456, p.pointee)
  expectEqual(789, p[789])
}

runAllTests()
// UNSUPPORTED: OS=windows-msvc
