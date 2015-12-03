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

extension UnsafeReference where Object : TestProtocol1 {
  var _instanceIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

var UnmanagedTests = TestSuite("UnsafeReference")

UnmanagedTests.test("init(bitPattern:)/trap")
  .skip(.Custom(
    { !_isDebugAssertConfiguration() },
    reason: "init(bitPattern:) does a _stdlibAssert() for null pointers"))
  .code {
  let null: OpaquePointer = getPointer(nil)
  expectCrashLater()
  let unmanaged = UnsafeReference<AnyObject>(bitPattern: null)
  _blackHole(unmanaged)
}

class FooClass {}

UnmanagedTests.test("unsafeBitCast(UnsafeReference, Int)") {
  let ref = FooClass()
  expectNotEqual(
    0,
    unsafeBitCast(
      UnsafeReference(withoutRetaining: ref) as UnsafeReference<AnyObject>,
      Int.self))
  _fixLifetime(ref)
}

runAllTests()

