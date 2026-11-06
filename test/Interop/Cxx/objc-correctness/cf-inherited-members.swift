// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -O)

// REQUIRES: executable_test
// REQUIRES: objc_interop

import CoreFoundation
import CFInheritedMembers
import StdlibUnittest

var CFInheritedMembersTestSuite = TestSuite("CFInheritedMembers")

func makeArray() -> CFMutableArray {
  return CFArrayCreateMutable(nil, 0, nil)
}

CFInheritedMembersTestSuite.test("CF_CONSUMED") {
  let a = makeArray()
  let start = CFGetRetainCount(a)
  for _ in 0..<10 { CFDerived().consume(a) }
  expectEqual(start, CFGetRetainCount(a))
}

CFInheritedMembersTestSuite.test("CF_RETURNS_NOT_RETAINED") {
  let a = makeArray()
  var d = CFDerived()
  d.arr = Unmanaged.passUnretained(a)
  let start = CFGetRetainCount(a)
  for _ in 0..<10 { _ = d.borrow() }
  expectEqual(start, CFGetRetainCount(a))
  withExtendedLifetime(d) {}
}

CFInheritedMembersTestSuite.test("CF_RETURNS_RETAINED") {
  let a = makeArray()
  var d = CFDerived()
  d.arr = Unmanaged.passUnretained(a)
  let start = CFGetRetainCount(a)
  for _ in 0..<10 { _ = d.copy() }
  expectEqual(start, CFGetRetainCount(a))
  withExtendedLifetime(d) {}
}

CFInheritedMembersTestSuite.test("CF_IMPLICIT_BRIDGING_ENABLED") {
  let a = makeArray()
  var d = CFDerived()
  d.arr = Unmanaged.passUnretained(a)
  let start = CFGetRetainCount(a)
  for _ in 0..<10 { _ = d.getAudited() }
  expectEqual(start, CFGetRetainCount(a))
  withExtendedLifetime(d) {}
}

runAllTests()
