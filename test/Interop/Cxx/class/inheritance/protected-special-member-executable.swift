// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default) | %FileCheck %s
//
// REQUIRES: executable_test
//
// REQUIRES: rdar168302720
// Fails on Windows：  https://github.com/swiftlang/swift/issues/67288
// Fails on non-macOS: https://github.com/swiftlang/swift/issues/86426

import StdlibUnittest
import ProtectedSpecialMember

var ProtectedSpecialMemberTestSuite = TestSuite("Protected Special Members")

ProtectedSpecialMemberTestSuite.test("class that inherits protected destructor") {
  var d = InheritsProtectedDtor()
  d.inDerived = 123
  expectEqual(123, d.inDerived)

  // FIXME: accessing fromBase should be fine, but doesn't work because we rely on
  // importing the base class to access its members.
  // d.fromBase = 456
  // expectEqual(456, d.fromBase)

  // Make sure inherited protected destructor is called:
  // CHECK: ~ProtectedDtor(fromBase = 111)
}

ProtectedSpecialMemberTestSuite.test("class that privately inherits protected destructor") {
  var d = PrivatelyInheritsProtectedDtor()
  d.inDerived = 123
  expectEqual(123, d.inDerived)

  d.setFromBase(456)

  // Make sure inherited protected destructor is called:
  // CHECK: ~ProtectedDtor(fromBase = 456)
}

ProtectedSpecialMemberTestSuite.test("class that inherits protected copy constructor") {
  let c1 = InheritsProtectedCopy()
  let c2 = c1
  expectEqual(111, c1.getFromBase())
  expectEqual(112, c2.getFromBase())

  var c3 = InheritsProtectedCopy()
  c3.setFromBase(66)
  let c4 = c3
  expectEqual(66, c3.getFromBase())
  expectEqual(67, c4.getFromBase())
}

ProtectedSpecialMemberTestSuite.test("classes that privately inherit protected copy constructor") {
  let c1 = PrivatelyInheritsProtectedCopy()
  let c2 = c1
  expectEqual(111, c1.getFromBase())
  expectEqual(112, c2.getFromBase())

  var c3 = PrivatelyInheritsProtectedCopy()
  c3.setFromBase(66)
  let c4 = c3
  expectEqual(66, c3.getFromBase())
  expectEqual(67, c4.getFromBase())

  let p = PrivatelyInheritsPrivatelyInheritsProtectedCopy()
  let _ = p
}

ProtectedSpecialMemberTestSuite.test("class that contains vector of class that inherited copy ctor") {
  let v1 = FieldVecOfInheritsProtectedCopy(101, 201, 301)
  expectEqual(101, v1.get(0))
  expectEqual(201, v1.get(1))
  expectEqual(301, v1.get(2))

  let v2 = v1 // Invokes copy ctor of std::vector<InheritsProtectedCopy>,
              // which invokes copy ctor of InheritsProtectedCopy elements,
              // thus incrementing each value

  expectEqual(102, v2.get(0))
  expectEqual(202, v2.get(1))
  expectEqual(302, v2.get(2))
}

ProtectedSpecialMemberTestSuite.test("class that inherits protected move constructor") {
  var m1 = InheritsProtectedMove()
  expectEqual(111, m1.getFromBase())
  m1.setFromBase(112)
  let m2 = m1
  expectEqual(112, m2.getFromBase())
}

ProtectedSpecialMemberTestSuite.test("class with public move and protected copy") {
  var cm1 = ProtectedCopyWithMove()
  expectEqual(111, cm1.fromBase)
  cm1.fromBase = 112
  let cm2 = cm1
  expectEqual(112, cm2.fromBase)
}

ProtectedSpecialMemberTestSuite.test("class that inherits public move and protected copy") {
  var c3 = InheritsProtectedCopyWithMove()
  c3.setFromBase(66)
  let c4 = c3
  expectEqual(66, c3.getFromBase())
  expectEqual(67, c4.getFromBase())
}

runAllTests()
