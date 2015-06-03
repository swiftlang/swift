// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// XFAIL: interpret

import StdlibUnittest

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

