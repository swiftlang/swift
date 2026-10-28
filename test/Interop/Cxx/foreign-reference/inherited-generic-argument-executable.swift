// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance -Xfrontend -disable-availability-checking)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

// This test asserts that a shared FRT's custom retain/release are actually
// called, which only holds at -Onone. At -O the balanced operations can be
// optimized away.
//
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size
// UNSUPPORTED: swift_test_mode_optimize_unchecked
// UNSUPPORTED: swift_test_mode_optimize_with_implicit_dynamic

import InheritedGenericArgument
import StdlibUnittest

var Tests = TestSuite("InheritedGenericArgument")

func classify<T: BaseObj>(_ obj: T) -> CInt { obj.tag() }

Tests.test("base-constrained generic with derived FRT argument") {
  expectEqual(1, classify(DerivedObj1.make()))
  expectEqual(2, classify(DerivedObj2.make()))
}

func classifyShared<T: SharedObj>(_ obj: T) -> CInt { obj.tag() }

func echoShared<T: SharedObj>(_ obj: T) -> T { obj }

Tests.test("custom retain/release through a shared FRT-bound generic") {
  expectEqual(0, SharedObj.numRefs())
  expectEqual(0, SharedObj.numDerefs())

  do {
    let a = SharedObj.make()
    expectEqual(1, a.currentRefCount())

    expectEqual(42, classifyShared(a))
    expectEqual(1, a.currentRefCount())

    do {
      let b = echoShared(a)
      expectEqual(42, b.tag())
      expectEqual(2, a.currentRefCount())
    }
    expectEqual(1, a.currentRefCount())
  }

  // Should have called C++ ref-counting methods instead of Swift native ones
  expectTrue(SharedObj.numRefs() > 0)
  expectTrue(SharedObj.numDerefs() > 0)

  // `a` is now out of scope, so the object should be fully released, meaning
  // derefs should exceed refs by exactly one
  expectEqual(SharedObj.numRefs() + 1, SharedObj.numDerefs())
}

runAllTests()
