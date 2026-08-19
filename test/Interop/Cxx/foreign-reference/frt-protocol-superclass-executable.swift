// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking)

// Regression test for the parameter convention of foreign reference types
// in a protocol witness.
//
// REQUIRES: executable_test
//
// This test asserts that a shared FRT's custom retain/release are actually
// called, which only holds at -Onone. At -O the balanced operations can be
// optimized away.
//
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size
// UNSUPPORTED: swift_test_mode_optimize_unchecked
// UNSUPPORTED: swift_test_mode_optimize_with_implicit_dynamic

import FRTProtocolSuperclass
import StdlibUnittest

var Tests = TestSuite("FRTProtocolSuperclass")

protocol TaggedViaImport: SharedBase {
  func tag() -> CInt
}

extension SharedBase: TaggedViaImport {}

protocol TaggedViaExtension: SharedBase {
  func label() -> CInt
}

extension SharedBase: TaggedViaExtension {
  func label() -> CInt { tag() }
}

func tagOf<T: TaggedViaImport>(_ obj: T) -> CInt { obj.tag() }
func echo<T: TaggedViaImport>(_ obj: T) -> T { obj }

func labelOf<T: TaggedViaExtension>(_ obj: T) -> CInt { obj.label() }
func echoLabelled<T: TaggedViaExtension>(_ obj: T) -> T { obj }

Tests.test("custom retain/release through an FRT-bound protocol witness") {
  expectEqual(0, SharedBase.numRefs())
  expectEqual(0, SharedBase.numDerefs())

  // Witness is the imported C++ method.
  do {
    // `make` is SWIFT_RETURNS_RETAINED, so the object arrives with refcount 1
    // and no call to ref().
    let a = SharedBase.make()
    expectEqual(1, a.currentRefCount())

    expectEqual(42, tagOf(a))
    expectEqual(1, a.currentRefCount())

    do {
      let b = echo(a)
      expectEqual(42, b.tag())
      expectEqual(2, a.currentRefCount())
    }
    expectEqual(1, a.currentRefCount())
  }

  // Witness is defined in a Swift extension. Uses a fresh object so the
  // per-object counts below start from a known state.
  do {
    let a = SharedBase.make()
    expectEqual(1, a.currentRefCount())

    expectEqual(42, labelOf(a))
    expectEqual(1, a.currentRefCount())

    do {
      let b = echoLabelled(a)
      expectEqual(42, b.label())
      expectEqual(2, a.currentRefCount())
    }
    expectEqual(1, a.currentRefCount())
  }

  // Should have called the C++ ref-counting methods instead of the Swift
  // native ones.
  expectTrue(SharedBase.numRefs() > 0)
  expectTrue(SharedBase.numDerefs() > 0)

  // Both objects are now out of scope and fully released, so derefs should
  // exceed refs by exactly two, one final release each.
  expectEqual(SharedBase.numRefs() + 2, SharedBase.numDerefs())
}

runAllTests()
