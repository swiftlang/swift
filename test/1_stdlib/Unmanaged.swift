// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// XFAIL: interpret

import StdlibUnittest

// Check that the generic parameter is called 'Instance'.
protocol TestProtocol1 {}

extension Unmanaged where Instance : TestProtocol1 {
  var _instanceIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

var UnmanagedTests = TestSuite("Unmanaged")

UnmanagedTests.test("fromOpaque()/trap")
  .skip(.Custom(
    { !_isDebugAssertConfiguration() },
    reason: "fromOpaque() does a _debugPrecondition() for null pointers"))
  .code {
  let null = getPointer(COpaquePointer())
  expectCrashLater()
  let unmanaged = Unmanaged<AnyObject>.fromOpaque(null)
  _blackHole(unmanaged)
}

class FooClass {}

UnmanagedTests.test("unsafeBitCast(Unmanaged, Word)") {
  let ref = FooClass()
  expectNotEqual(
    0,
    unsafeBitCast(
      Unmanaged.passUnretained(ref) as Unmanaged<AnyObject>,
      Word.self))
  _fixLifetime(ref)
}

runAllTests()

