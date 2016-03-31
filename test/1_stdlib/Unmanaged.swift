// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// XFAIL: interpret

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

// Check that the generic parameter is called 'Instance'.
protocol TestProtocol1 {}

extension Unmanaged where Instance : TestProtocol1 {
  var _instanceIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

var UnmanagedTests = TestSuite("Unmanaged")

UnmanagedTests.test("fromOpaque()/trap")
  .skip(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "init(bitPattern:) does a _debugPrecondition() for null pointers"))
  .code {
  let null: OpaquePointer = getPointer(nil)
  expectCrashLater()
  let unmanaged = Unmanaged<AnyObject>.fromOpaque(null)
  _blackHole(unmanaged)
}

class FooClass {}

UnmanagedTests.test("unsafeBitCast(Unmanaged, Int)") {
  let ref = FooClass()
  expectNotEqual(
    0,
    unsafeBitCast(
      Unmanaged.passUnretained(ref) as Unmanaged<AnyObject>,
      to: Int.self))
  _fixLifetime(ref)
}

class Foobar {
  func foo() -> Int { return 1 }
}

UnmanagedTests.test("_withUnsafeGuaranteedRef") {
  var ref = Foobar()
  var unmanaged = Unmanaged.passUnretained(ref)
  withExtendedLifetime(ref) {
    unmanaged._withUnsafeGuaranteedRef {
      expectTrue(ref === $0)
    }
    unmanaged._withUnsafeGuaranteedRef {
      expectEqual(1, $0.foo())
    }
  }
}

runAllTests()

